
train_mlp <- function( y,
                 lags = 1:5,
                 seas_dummy = TRUE,
                 index_dummy = TRUE,
                 intercept = FALSE,
                 scaler = scaler_min_max,
                 inv_scaler = scaler_inverse_min_max,
                 scaler_args = list( a = -0.8, b = 0.8 ),
                 n_diffs = NULL,
                 n_hidden = 5,
                 reps = 15,
                 combination_fun = median) {

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

  colnames(X) <- paste0("feature_",seq_len(ncol(X)))
  df = data.frame( y_net = scaled_y, X )
  formula = as.formula(paste0(  "y_net ~", paste0(colnames(X), collapse = "+")))

  model <- neuralnet::neuralnet( formula, data = df, hidden = n_hidden, rep = reps,
                                 err.fct = "sse", linear.output = TRUE )
  fitted <- do.call( cbind, model[["net.result"]])
  fitted <- apply( fitted, 1, combination_fun )
  fitted <- fitted_transformers$inverse_scaler( fitted )

  structure( list(
    data = y,
    fitted = c(rep(NA,max(lags)),fitted),
    transform_fit = fitted_transformers,
    transformers = transformers,
    model = model,
    reps = reps,
    combination_fun = combination_fun
  ), class = "MLP")
}

forecast.MLP <- function( object, h = 8, ...  ) {

  y <- object$transform_fit$y
  trained_transformers <- object$transform_fit
  forecast_prep <- object$transformers$forecast_prep

  model <- object$model
  reps <- object$reps
  combination_fun <- object$combination_fun

  all_forecasts <- list()
  for( rep in seq_len(reps) ) {
    forecast <- rep(NA, h)
    for( step in seq_len(h)) {
      y_ <- c(y, forecast[seq_len(step-1)])
      new_x <- forecast_prep(y_, h, trained_transformers)

      colnames(new_x) <- paste0("feature_",seq_len(ncol(new_x)))
      # return(new_x)
      forecast[step] <- c(predict( model, new_x, rep = rep ))
    }
    all_forecasts[[rep]] <- forecast
  }

  all_forecasts <- purrr::map( all_forecasts,
                               ~ trained_transformers$inverse_scaler( .x ))
  # combine using a median for now
  forecast <- do.call(cbind, all_forecasts)
  forecast <- apply( forecast, 1, combination_fun )
  return(forecast)
}
