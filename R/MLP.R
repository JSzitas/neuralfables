
MLP <- function( y,
                 lags = 1:5,
                 seas_dummy = TRUE,
                 index_dummy = TRUE,
                 n_hidden = 5,
                 reps = 15,
                 combination_fun = median) {
  y <- min_max(y, a = -0.8, b = 0.8)
  y_ <- y$x
  X  <- make_lag_matrix( y_, lags = lags )

  if( seas_dummy ) {
    seasonalities <- find_seasonalities(y_)
    if( length(seasonalities) > 0 ){
      X_seas <- trigonometric_seasonal_dummy( length(y_), seasonalities )
      X <- cbind( X, X_seas )
    }
    else{
      seasonalities <- NULL
    }
  }
  if( index_dummy ) {
    X_index <- time_dummy(length(y_))
    X <- cbind( X, X_index )
  }
  else {
    X_index = NULL
  }

  X <- na.omit(X)
  colnames(X) <- paste0("feature_",seq_len(ncol(X)))
  y_net = y_[ (max(lags)+1) : length(y_) ]

  formula = as.formula(paste0(  "y_net ~", paste0(colnames(X), collapse = "+")))
  df = data.frame( y_net = y_net, X )

  model <- neuralnet::neuralnet( formula, data = df, hidden = n_hidden, rep = reps,
                                 err.fct = "sse", linear.output = TRUE )
  fitted <- do.call( cbind, model[["net.result"]])
  fitted <- apply( fitted, 1, combination_fun )
  fitted <- inverse_min_max( fitted, y$min_x, y$max_x, y$a, y$b )

  structure( list(
    data = y_,
    scale_args = y,
    lags = lags,
    model = model,
    reps = reps,
    fitted = fitted,
    seasonalities = seasonalities,
    index_dummy = X_index,
    combination_fun = combination_fun
  ), class = "MLP")
}

forecast.MLP <- function( object, h = 8, ...  ) {

  y <- object$data
  min_x <- object$scale_args$min_x
  max_x <- object$scale_args$max_x
  scale_a <- object$scale_args$a
  scale_b <- object$scale_args$b
  lags <- object$lags
  seasonalities <- object$seasonalities
  X_index <- object$index_dummy
  model <- object$model
  reps <- object$reps
  combination_fun <- object$combination_fun

  if( !is.null(seasonalities) ) {
    X_seas <- trigonometric_seasonal_dummy( n = length(y) + h,
                                            seas_length = seasonalities)
  }

  all_forecasts <- list()
  # append NA to allow lagging

  for( rep in seq_len(reps) ) {
    forecast <- rep(NA, h)
    for( step in seq_len(h)) {
      y_ <- c(y, forecast[seq_len(step-1)])
      new_x <- matrix( rev(tail( y_, max(lags) ))[lags], nrow = 1)

      if( !is.null(seasonalities) ) {
        last_index <- X_index[nrow(X_index),]
        last_index <- matrix( last_index + c( 1, -1 ), nrow = 1)
        curr_seas <- matrix( X_seas[ length(y) + 1,], nrow = 1)
        new_x <- cbind( new_x, curr_seas )
      }
      if( !is.null(X_index) ) {
        last_index <- matrix( last_index + c( 1, -1 ), nrow = 1)
        new_x <- cbind( new_x, last_index )
      }
      colnames(new_x) <- paste0("feature_",seq_len(ncol(new_x)))
      forecast[step] <- c(predict( model, new_x, rep = rep ))
    }
    all_forecasts[[rep]] <- forecast
  }

  all_forecasts <- purrr::map( all_forecasts, ~ inverse_min_max(.x, min_x, max_x, scale_a, scale_b ))
  # combine using a median for now
  forecast <- do.call(cbind, all_forecasts)
  forecast <- apply( forecast, 1, combination_fun )
  return(forecast)
}
