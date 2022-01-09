
train_elm <- function( y,
                       n_hidden = 5,
                       activation = sigmoid,
                       lags = 1:5,
                       seas_dummy = TRUE,
                       index_dummy = TRUE,
                       intercept = FALSE,
                       scaler = scaler_min_max,
                       inv_scaler = scaler_inverse_min_max,
                       scaler_args = list(a = -0.8, b = 0.8),
                       n_diffs = NULL ) {
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
  # create random weights from a normal distribution (runif might be better)
  # runif version from nnfor
  limits <- c(-1, 1) * (1 / sqrt(ncol(X)-1))
  W <- matrix( runif( ncol(X) * n_hidden,
                      min = limits[1],
                      max = limits[2]),
              nrow = ncol(X),
              ncol = n_hidden
  )
  # call activation on the projection to get the hidden layer
  H <- activation(  X %*% W )
  # compute weights for hidden layer - the only actual training step
  B <- MASS::ginv(H) %*% scaled_y

  fitted <- H %*% B

  structure( list(
    data = y,
    fitted = c(rep(NA, max(lags)), fitted),
    transform_fit = fitted_transformers,
    transformers = transformers,
    W = W,
    B = B,
    activation = activation
  ), class = "ELM")
}

forecast.ELM <- function( object, h = 8, ...  ) {

  y <- object$transform_fit$y
  trained_transformers <- object$transform_fit
  forecast_prep <- object$transformers$forecast_prep

  B <- object$B
  W <- object$W
  activation <- object$activation

  forecast <- rep(NA, h)
  for( step in seq_len(h)) {
    y_ <- c(y, forecast[seq_len(step - 1)])
    new_x <- forecast_prep(y_, h, trained_transformers)
    # transform via projection + activation
    H <- activation(  new_x %*% W )
    # make forecast
    forecast[step] <- c(H %*% B)
  }
  return(trained_transformers$inverse_scaler(forecast))
}
