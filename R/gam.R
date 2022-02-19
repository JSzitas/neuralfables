




train_gam <- function( y,
                       lags = 1:5,
                       xreg = NULL,
                       seas_dummy = TRUE,
                       index_dummy = FALSE,
                       intercept = FALSE,
                       scaler = scaler_min_max,
                       inv_scaler = scaler_inverse_min_max,
                       scaler_args = list( a = -0.8, b = 0.8 ),
                       n_diffs = NULL
) {

  transformers <- make_X_transformers( lags = lags,
                                       xreg = xreg,
                                       xreg_lags = NULL,
                                       seas_dummy,
                                       index_dummy,
                                       intercept,
                                       scaler = scaler,
                                       scaler_args = scaler_args,
                                       inverse_scaler = inv_scaler,
                                       n_diffs = n_diffs)

  fitted_transformers <- transformers$train_prep(y)
  scaled_y <- fitted_transformers$y
  X <- fitted_transformers$X

  df <- data.frame( scaled_y, X )
  colnames(df) <- c("target", paste0( "feature_", seq_len(ncol(df)-1 )))

  # smooth splines for all features except the first one - which is an intercept
  formula <- paste0( "s(feature_", (1+intercept):(ncol(df)-1),")")
  # collapse formula terms (add them together)
  formula <- paste0( formula, collapse = "+" )
  # add target and intercept
  formula <- if( intercept ){ paste0( "target ~ feature_1 + ", formula ) }
                        else{ paste0( "target ~ ", formula ) }
  # actually cast the formula to a formula object
  formula <- stats::as.formula(formula)

  model <- mgcv::gam(  formula, data = df )
  fitted <- fitted(model)
  fitted <- fitted_transformers$inverse_scaler( fitted )
  fitted <- c(rep(NA,max(lags)),fitted)

  structure( list( data = y,
                   # the one NA is due to the shift in the ESN by 1 observation
                   fitted = fitted,
                   resid = y - fitted,
                   transform_fit = fitted_transformers,
                   transformers = transformers,
                   model = model),
             class = "GAM")
}

forecast.GAM <- function( object, h = 8, ... ) {

  y <- object$transform_fit$y
  forecast_prep <- object$transformers$forecast_prep
  trained_transformers <- object$transform_fit
  model <- object$model

  forecast <- rep(NA, h)
  sd_errs <- rep(NA, h)
  for (step in seq_len(h) ) {
    y_ <- c(y, forecast[seq_len(step - 1)])
    new_x <- as.data.frame(forecast_prep(y_, h, trained_transformers))
    colnames(new_x) <- paste0( "feature_", seq_len(ncol(new_x) ))
    forecast[step] <- stats::predict( model, new_x )
  }
  return( trained_transformers$inverse_scaler(forecast) )
}
