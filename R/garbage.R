
orf_features <- function(n_garbage, scaling = 0.1) {
  d <- n_garbage
  # qr decomposition
  Q <- qr.Q(qr(
    matrix(stats::rnorm(n = d^2), nrow = d)
  ))

  # this is the chi, not the chi squared (I misread and this was a giant
  # source of a headache)
  S <- sqrt(stats::rchisq(
    n = d,
    df = d
  ))

  W <- scaling * S * Q
  return(W)
}

# D = ncol(X)
# d = the number of garbage features
orf <- function(X, d, scaling = 0.1) {
  D <- ncol(X)

  if (D > d) {
    repetitions <- ceiling(D / d) - 1
    W <- orf_features(d, scaling = scaling)
    while (repetitions > 0) {
      W <- rbind(W, orf_features(d, scaling = scaling))
      repetitions <- repetitions - 1
    }
    W <- W[1:D, ]
  } else {
    # D < d
    W <- orf_features(d, scaling = scaling)
    W <- W[1:D, ]
  }
  return(W)
}

rff <- function(X, d, scaling = 0.1) {
  W <- scaling * matrix(stats::rnorm(n = d * ncol(X)), nrow = ncol(X))
  return(W)
}


train_garbage <- function(y,
                          n_garbage = 20,
                          garbage_type = c(orf, rff),
                          lambda = 1,
                          lags = 1:5,
                          seas_dummy = TRUE,
                          index_dummy = TRUE,
                          intercept = FALSE,
                          scaler = scaler_min_max,
                          inv_scaler = scaler_inverse_min_max,
                          scaler_args = list(a = -0.8, b = 0.8),
                          n_diffs = NULL) {
  transformers <- make_X_transformers(
    lags,
    xreg = NULL,
    xreg_lags = NULL,
    seas_dummy,
    index_dummy,
    intercept,
    scaler,
    scaler_args,
    inv_scaler,
    n_diffs
  )
  fitted_transformers <- transformers$train_prep(y)
  X <- fitted_transformers$X
  scaled_y <- fitted_transformers$y

  W <- garbage_type[[1]](X, d = n_garbage, scaling = 0.5)
  Z <- X %*% W
  Z <- cbind(cos(Z), sin(Z))

  model <- ridge_solver( scaled_y, Z, lambda = lambda, family = "gaussian" )
  preds <- c( Z %*% model[["coef"]] )
  fitted <- fitted_transformers$inverse_scaler(preds)
  fitted <- c(rep(NA, max(lags)), fitted)

  structure(list(
    data = y,
    fitted = fitted,
    resid = y - fitted,
    transform_fit = fitted_transformers,
    transformers = transformers,
    model = model,
    W = W
  ), class = "garbage")
}

forecast.garbage <- function(object, h = 8, ...) {
  y <- object$transform_fit$y
  trained_transformers <- object$transform_fit
  forecast_prep <- object$transformers$forecast_prep

  W <- object$W
  model <- object$model

  forecast <- rep(NA, h)
  for (step in seq_len(h)) {
    y_ <- c(y, forecast[seq_len(step - 1)])
    new_x <- forecast_prep(y_, h, trained_transformers)
    # form kernel, forecast using learned coefficients
    Z <- new_x %*% W
    Z <- cbind(cos(Z), sin(Z))
    forecast[step] <- stats::predict( model, Z)
  }
  return(trained_transformers$inverse_scaler(forecast))
}
