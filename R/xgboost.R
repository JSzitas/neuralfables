
train_xgboost <- function(y,
                          xgboost_control = list(
                            max_depth = 5, eta = 0.3, nrounds = 100,
                            nthread = 1, objective = "reg:tweedie", verbose = 0
                          ),
                          lags = 1:4,
                          xreg = NULL,
                          xreg_lags = NULL,
                          seas_dummy = TRUE,
                          n_diffs = NULL,
                          ...) {
  transformers <- make_X_transformers(lags,
    xreg,
    lags,
    seas_dummy,
    index_dummy = FALSE,
    intercept = FALSE,
    scaler = scaler_identity,
    scaler_args = list(),
    inverse_scaler = scaler_inverse_identity,
    n_diffs
  )

  fitted_transformers <- transformers$train_prep(y)
  X <- fitted_transformers$X
  y_ <- fitted_transformers$y

  colnames(X) <- paste0("feature_", seq_len(ncol(X)))
  df <- xgboost::xgb.DMatrix(data = X, label = y_)

  model <- do.call(
    xgboost::xgboost,
    c(list(data = df), xgboost_control)
  )

  fitted <- c( rep(NA, max(lags)),
               stats::predict(model, df)
  )

  structure(list(
    data = y,
    fitted = fitted,
    resid = y - fitted,
    transform_fit = fitted_transformers,
    transformers = transformers,
    model = model
  ), class = "XGBOOST")
}

forecast.XGBOOST <- function(object, h = 8, ...) {
  y <- object$transform_fit$y
  trained_transformers <- object$transform_fit
  forecast_prep <- object$transformers$forecast_prep
  model <- object$model

  forecast <- rep(NA, h)
  for (step in seq_len(h)) {
    y_ <- c(y, forecast[seq_len(step - 1)])
    new_x <- forecast_prep(y_, h, trained_transformers, ...)
    colnames(new_x) <- paste0("feature_", seq_len(ncol(new_x)))
    new_x <- xgboost::xgb.DMatrix(data = new_x )
    forecast[step] <- c(stats::predict(model, new_x))
  }

  return(forecast)
}
