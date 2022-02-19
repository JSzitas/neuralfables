


train_glm <- function( y,
                       lags = 1:5,
                       lambda = 0.05,
                       family = "gaussian",
                       seas_dummy = TRUE,
                       index_dummy = FALSE,
                       intercept = TRUE,
                       n_diffs = NULL ) {
    transformers <- make_X_transformers( lags,
                                         xreg = NULL,
                                         xreg_lags = NULL,
                                         seas_dummy,
                                         index_dummy,
                                         intercept,
                                         scaler_identity,
                                         list(),
                                         scaler_inverse_identity,
                                         n_diffs)

    fitted_transformers <- transformers$train_prep(y)
    X <- fitted_transformers$X
    scaled_y <- fitted_transformers$y

    # compute weights for hidden layer - the only actual training step
    model <- ridge_solver( scaled_y, X, lambda = lambda, family = family )
    fitted <- X %*% model[["coef"]]
    fitted <- fitted_transformers$inverse_scaler( fitted )
    fitted <- c(rep(NA, max(lags)), fitted)

    structure( list(
      data = y,
      fitted = fitted,
      resid = y - fitted,
      transform_fit = fitted_transformers,
      transformers = transformers,
      family = family,
      model = model
    ), class = "tsGLM")
  }

forecast.tsGLM <- function( object, h = 8, ...  ) {

  y <- object$transform_fit$y
  trained_transformers <- object$transform_fit
  forecast_prep <- object$transformers$forecast_prep

  model <- object$model
  W <- object$W
  activation <- object$activation

  forecast <- rep(NA, h)
  for( step in seq_len(h)) {
    y_ <- c(y, forecast[seq_len(step - 1)])
    new_x <- forecast_prep(y_, h, trained_transformers)
    # make forecast
    forecast[step] <- predict( model, new_x )
  }
  return(trained_transformers$inverse_scaler(forecast))
}
