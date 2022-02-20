
train_mboost <- function( y,
                       family = mboost::Gaussian(),
                       boost_control = mboost::boost_control( mstop = 100, nu = 0.1 ),
                       tree_controls = partykit::ctree_control(
                         teststat = "quad", testtype = "Teststatistic",
                         mincriterion = 0, minsplit = 10, minbucket = 4,
                         maxdepth = 1, saveinfo = FALSE),
                       lags = 1:4,
                       xreg = NULL,
                       xreg_lags = NULL,
                       seas_dummy = TRUE,
                       n_diffs = NULL,
                       ...) {

  transformers <- make_X_transformers( lags,
                                       xreg,
                                       lags,
                                       seas_dummy,
                                       index_dummy = FALSE,
                                       intercept = FALSE,
                                       scaler = scaler_identity,
                                       scaler_args = list(),
                                       inverse_scaler = scaler_inverse_identity,
                                       n_diffs)

  fitted_transformers <- transformers$train_prep(y)
  X <- fitted_transformers$X
  y_ <- fitted_transformers$y

  colnames(X) <- paste0("feature_",seq_len(ncol(X)))
  df = data.frame( y_boost = y_, X )
  formula = stats::as.formula(paste0(  "y_boost ~", paste0(colnames(X), collapse = "+")))

  model <- mboost::blackboost( formula, data = df, control = boost_control, tree_controls = tree_controls,
                      family = family )
  fitted <- fitted(model)
  fitted <- c(rep(NA,max(lags) + length(y) - (length(fitted) + max(lags))), fitted)

  structure( list(
    data = y,
    fitted = fitted,
    resid =  y-fitted,
    transform_fit = fitted_transformers,
    transformers = transformers,
    model = model
  ), class = "MBOOST")
}

forecast.MBOOST <- function( object, h = 8, ...  ) {

  y <- object$transform_fit$y
  trained_transformers <- object$transform_fit
  forecast_prep <- object$transformers$forecast_prep

  model <- object$model

  forecast <- rep(NA, h)
  for( step in seq_len(h)) {
    y_ <- c(y, forecast[seq_len(step-1)])
    new_x <- forecast_prep(y_, h, trained_transformers, ...)
    colnames(new_x) <- paste0("feature_",seq_len(ncol(new_x)))
    forecast[step] <- c(stats::predict( model, new_x, type = "response" ))
  }

  return(forecast)
}
