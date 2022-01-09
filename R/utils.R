# check_trend_season <- function( .data ) {
#   target <- rlang::sym(tsibble::measured_vars(.data)[1])
#   mdl <- fabletools::model( .data, fable::ETS( !!target ) )
#   selected_model <- fabletools::model_sum(test[[1]][[1]][["fit"]])
#   # this looks for the trend of the fitted ETS( error, TREND, season ) model -
#   # where N means "no-trend" - in every other case, we assume there is a trend.
#   trend <- !grepl( pattern = ",N,", x = selected_model )
#   # ths looks for the seasonality of the fitted ETS(error, trend, SEASON ) model
#   # where N means, again, "no-season"
#   season <- !grepl( pattern = ",[A-za-z],N", x = selected_model )
#   return( list(trend = trend, season = season) )
# }

# kdemode <- function(data){
#
#   # Fix from/to
#   from <- min(data)-0.1*diff(range(data))
#   to <- max(data)+0.1*diff(range(data))
#
#   # Calculate KDE
#   ks <- density(data,bw="SJ",n=512,from=from,to=to)
#   x <- ks$x
#   f <- ks$y
#   h <- ks$bw
#
#   # Find mode
#   mo <- x[which(f==max(f))][1] # mode
#
#   return(list(mode=mo,xd=x,fd=f,h=h))
# }

scaler_min_max <- function( x, a = -0.8, b = 0.8  ) {

  min_x = min(x, na.rm = TRUE)
  max_x = max(x, na.rm = TRUE)

  list( x = ( b - a ) * ( x - min_x )/( max_x - min_x ) + a,
        args = list( a = a,
        b = b,
        min_x = min_x,
        max_x = max_x ))
}

scaler_inverse_min_max <- function( x, min_x = 0, max_x = 1, a = -1, b = 1  ) {
  # (max_x - min_x )*(x - min(x))/( max(x) - min(x)) + min_x
  ( max_x - min_x )*( x - a )/( b - a ) + min_x
}

scaler_identity <- function( x ) {
  list(x = x)
}

scaler_inverse_identity <- function( x ) {
  x
}

resolve_intercept <- function( x, p_threshold = 0.05, use_intercept = NULL ) {
  if( isTRUE(use_intercept) ) {
    return(TRUE)
  }
  if(  wilcox.test( x )$p.value <= p_threshold ) {
    return(TRUE)
  }
  FALSE
}

make_lag_matrix <- function( y, lags = 1:5 ) {
  lags <- purrr::map( lags, function(lag) {
    dplyr::lag( y, lag )
  })
  do.call(cbind,lags)
}

sigmoid <- function(x) {
  y <- x / (1 + abs(x))
  return(y)
}

apply_differences <- function( y, differences = 0 ) {
  if( differences != 0 ) {
    integration_constants <- rep(NA, length(differences))
    for( d in seq_len(differences) ) {
      integration_constants[d] <- y[1]
      y <- diff( y, differences = differences[d] )
    }
  }
  else {
    integration_constants = 0
  }
  return(list( y = y,
               differences = differences,
               integration_constants = integration_constants)
  )
}

undifference <- function( y, differences, constants ) {
  for( d in seq_len(diifferences) ) {
    y <- diffinv( y, differences = differences[d], xi = constants[d]  )
  }
  return(y)
}

get_ndiffs <- function( y, ndiffs = NULL ) {
  if(!is.null(ndiffs)){
    return(ndiffs)
  }
  ndiffs <- purrr::safely( feasts::unitroot_ndiffs, otherwise = 0)(y)
  ndiffs <- c( ndiffs,
               purrr::safely(feasts::unitroot_nsdiffs, otherwise = 0)(y)  )
  unique( unlist(ndiffs) )
}

# create a closure which can be applied to both traning and forecasting data
make_X_transformers <- function( lags = NULL,
                                 xreg = NULL,
                                 xreg_lags = NULL,
                                 seas_dummy = TRUE,
                                 index_dummy = TRUE,
                                 intercept = NULL,
                                 scaler = min_max,
                                 scaler_args = list(a = -0.8, b = 0.8),
                                 inverse_scaler = inverse_min_max,
                                 n_diffs = NULL ) {

  if(is.null(lags)) {
    lags = 1:4
  }

  train_prep = function( y, xreg = NULL ) {

    scaling_result <- do.call( scaler, c( list(y), scaler_args))
    scaled_y <- scaling_result$x
    # check for differences
    ndiffs <- get_ndiffs(y, n_diffs)
    differencing_result <- apply_differences( scaled_y, ndiffs )
    scaled_y <- differencing_result$y

    # create matrix of lags
    X  <- make_lag_matrix( scaled_y, lags = lags )

    if( seas_dummy ) {
      seasonalities <- find_seasonalities(scaled_y)
      if( length(seasonalities) > 0 ){
        X_seas <- trigonometric_seasonal_dummy( length(scaled_y), seasonalities )
        X <- cbind( X, X_seas )
      }
      else{
        seasonalities <- NULL
      }
    }
    else{
      seasonalities <- NULL
    }
    if( index_dummy ) {
      X_index <- time_dummy(length(scaled_y))
      X <- cbind( X, X_index )
    }
    else {
      X_index = NULL
    }
    # check post scaled data for intercept and add intercept as necessary
    intercept <- resolve_intercept( scaled_y, use_intercept = intercept )
    if(intercept) {
      X <- cbind( 1, X)
    }
    if( !is.null(xreg) ) {
      # the xreg values only get scaled once, so we dont really care about keeping the scaler
      xreg <- apply( xreg, 2, function(xreg_col){
        do.call( scaler, c( list(xreg_col), scaler_args))
      }, simplify = FALSE)
      xreg <- do.call( cbind, purrr::map( xreg, ~ .$x))
      colnames(xreg) <- paste0("xreg_", seq_len(ncol(xreg)))
      X <- cbind( X, xreg )
    }
    X <- na.omit(X)
    return(list( y = scaled_y[(max(lags)+1) : length(y)],
                 X = X,
                 seasonalities = seasonalities,
                 index = X_index,
                 intercept = intercept,
                 xreg = xreg,
                 differencing = differencing_result,
                 scaler = scaler,
                 scaler_args = scaler_args,
                 inverse_scaler = do.call( purrr::partial,
                                           c(
                                             list( .f = inverse_scaler),
                                             scaling_result$args
                                             )
                                           )
                 )
           )
  }

  forecast_prep = function( y, h, train_result, xreg = NULL ) {

    new_x <- matrix( rev(tail( y, max(lags) ))[lags], nrow = 1)

    index <- train_result$index
    seasonalities <- train_result$seasonalities
    scaler <- train_result$scaler
    scaler_args <- train_result$scaler_args
    intercept <- train_result$intercept

    if( !is.null( seasonalities ) ) {
      X_seas <- trigonometric_seasonal_dummy( n = length(y) + h,
                                              seas_length = seasonalities)
      curr_seas <- matrix( X_seas[ length(y) + 1,], nrow = 1)
      new_x <- cbind( new_x, curr_seas )

    }
    if( !is.null(train_result$index)) {
      last_index <- matrix( index[nrow(index)] + h*c( 1, -1 ), nrow = 1)
      new_x <- cbind( new_x, last_index )
    }
    if( intercept ) {
      new_x <- cbind( 1, new_x )
    }
    if( !is.null(xreg) ) {
      xreg <- apply( xreg, 2, function(xreg_col){
        do.call( scaler, c( list(xreg_col), scaler_args))
      }, simplify = FALSE)
      xreg <- do.call( cbind, purrr::map( xreg, ~ .$x))
      colnames(xreg) <- paste0("xreg_", seq_len(ncol(xreg)))
      X <- cbind( X, xreg )
    }
    return(new_x)
  }
  return( list(train_prep = train_prep, forecast_prep = forecast_prep) )
}
