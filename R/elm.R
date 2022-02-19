
elm_trainer <- function( y, X, n_hidden = 5, activation = sigmoid, lambda = NULL ) {
  # create random weights from a normal distribution (runif might be better)
  # runif version from nnfor
  limits <- c(-0.8, 0.8) * (1 / sqrt(ncol(X)-1))
  W <- matrix( stats::runif( ncol(X) * n_hidden,
                             min = limits[1],
                             max = limits[2]),
               nrow = ncol(X),
               ncol = n_hidden
  )
  # call activation on the projection to get the hidden layer
  H <- activation(  X %*% W )
  # compute weights for hidden layer - the only actual training step
  model <- ridge_solver( y, H, lambda = lambda, family = "gaussian" )
  fitted <- c(H %*% model[["coef"]])
  return(list( model = list( model = model,
                             W = W),
               fitted = fitted )
         )
}

train_elm <- function( y,
                       n_hidden = 20,
                       activation = sigmoid,
                       lags = 1:5,
                       lambda = 0.05,
                       seas_dummy = TRUE,
                       index_dummy = FALSE,
                       intercept = FALSE,
                       scaler = scaler_min_max,
                       inv_scaler = scaler_inverse_min_max,
                       scaler_args = list(a = -0.8, b = 0.8),
                       n_diffs = 0,
                       reps = 15,
                       combination_fun = stats::median,
                       ...) {
  transformers <- make_X_transformers( lags,
                                       xreg = NULL,
                                       xreg_lags = NULL,
                                       seas_dummy,
                                       index_dummy,
                                       intercept,
                                       scaler,
                                       scaler_args,
                                       inv_scaler,
                                       n_diffs)

  fitted_transformers <- transformers$train_prep(y)
  X <- fitted_transformers$X
  scaled_y <- fitted_transformers$y

  # fit **reps** total number of models (due to inherent randomness of the process)
  models <- list()
  for(rep in seq_len(reps)) {
    models[[rep]] <- elm_trainer( scaled_y, X, n_hidden, activation, lambda = lambda )
  }

  model <- purrr::map( models, ~ .x[["model"]] )
  fits <- purrr::map( models, ~ .x[["fitted"]] )
  fits <- do.call( cbind, fits )
  fitted <- apply( fits, 1, combination_fun )
  fitted <- transformers$postprocessor( fitted, fitted_transformers)

  structure( list(
    data = y,
    fitted = fitted,
    resid = y - fitted,
    transform_fit = fitted_transformers,
    transformers = transformers,
    model = model,
    activation = activation,
    combination_fun = combination_fun
  ), class = "ELM")
}

forecast.ELM <- function( object, h = 8, ...  ) {

  y <- object$transform_fit$y
  trained_transformers <- object$transform_fit
  forecast_prep <- object$transformers$forecast_prep
  transformers <- object$transformers
  activation <- object$activation
  constant <- last(object$data)
  reps <- length(object$model)
  combination_fun <- object$combination_fun

  models <- object$model

  forecasts <- list()
  for( rep in seq_len(reps) ) {
    # select current model
    model <- models[[rep]][["model"]]
    W <- models[[rep]][["W"]]
    # generate forecasts
    forecast <- rep(NA, h)
    for( step in seq_len(h)) {
      y_ <- c(y, forecast[seq_len(step - 1)])
      new_x <- forecast_prep(y_, h, trained_transformers)
      # transform via projection + activation
      H <- activation(  new_x %*% W )
      # make forecast
      forecast[step] <- stats::predict( model, H )
    }
    # add forecasts to list
    forecasts[[rep]] <- forecast
  }

  forecast <- do.call( cbind, forecasts )
  forecast <- apply( forecast, 1, combination_fun )

  return(transformers$postprocessor(forecast, trained_transformers, add_nas = FALSE))
}
