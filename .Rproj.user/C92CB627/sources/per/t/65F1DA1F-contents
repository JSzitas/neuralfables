
#
# GP <- function( y,
#                 covariance = ,
#                 mean_fun = ,
#                 lags = 1:5,
#                 seas_dummy = TRUE,
#                 index_dummy = TRUE,
#                 lambda = 1e-8) {
#
#   y <- min_max(y)
#   min_x <- y$min_x
#   max_x <- y$max_x
#   y <- y$x
#   X  <- make_lag_matrix( y, lags = lags )
#
#   seasonalities <- NULL
#   if( seas_dummy ) {
#     seasonalities <- find_seasonalities(y)
#     if( length(seasonalities) > 0 ){
#       X_seas <- trigonometric_seasonal_dummy( length(y), seasonalities )
#       X <- cbind( X, X_seas )
#     }
#     else{
#       seasonalities <- NULL
#     }
#   }
#   if( index_dummy ) {
#     X_index <- time_dummy(length(y))
#     X <- cbind( X, X_index )
#   }
#
#   # X <- do.call( cbind, list( X, X_seas, X_index)  )
#   X <- na.omit(X)
#   W <- rff(X, d = n_garbage, scaling = 0.5 )
#   Z <- X %*% W
#   Z <- cbind(cos(Z), sin(Z))
#
#   coef <- y[ (max(lags)+1) : length(y) ] %*% Z  %*% solve( t(Z) %*% Z + lambda*diag(ncol(Z)) )
#
#   preds <- coef %*% t(Z)
#
#   structure( list(
#     data = y,#inverse_min_max( y, min_x = min_x, max_x = max_x),
#     min_x = min_x,
#     max_x = max_x,
#     lags = lags,
#     coef = coef,
#     fitted = c( rep(NA, max(lags)), inverse_min_max( preds, min_x = min_x, max_x = max_x)),
#     seasonalities = seasonalities,
#     index_dummy = X_index,
#     W = W
#   ), class = "garbage")
# }
#
# forecast.garbage <- function( object, h = 8, ...  ) {
#
#   y <- object$data
#   min_x <- object$min_x
#   max_x <- object$max_x
#   lags <- object$lags
#   coef <- object$coef
#   seasonalities <- object$seasonalities
#   X_index <- object$index_dummy
#   W <- object$W
#
#   if( !is.null(seasonalities) ) {
#     X_seas <- trigonometric_seasonal_dummy( n = length(y) + h,
#                                             seas_length = seasonalities)
#   }
#   last_index <- X_index[nrow(X_index),]
#
#   forecast <- rep(NA, h)
#   for( step in seq_len(h)) {
#
#     new_x <- purrr::map( lags, function(lag){
#       dplyr::lag( c(y, NA), lag)
#     })
#     new_x <- do.call( cbind, new_x )
#     new_x <- matrix( new_x[nrow(new_x),], nrow = 1)
#     last_index <- matrix( last_index + c( 1, -1 ), nrow = 1)
#     if( !is.null(seasonalities) ) {
#       curr_seas <- matrix( X_seas[ length(y) + 1,], nrow = 1)
#       new_x <- cbind( new_x, curr_seas )
#     }
#     if( !is.null(X_index) ) {
#       last_index <- matrix( last_index + c( 1, -1 ), nrow = 1)
#       new_x <- cbind( new_x, last_index )
#     }
#     # return(new_x)
#     Z <- new_x %*% W
#     Z <- cbind(cos(Z), sin(Z))
#     forecast[step] <- coef %*% t(Z)
#     y <- c(y, forecast[step])
#   }
#   # return(list( forecast, min_x, max_x ))
#   # return(forecast)
#   forecast <- inverse_min_max(forecast, min_x, max_x)
#
#   forecast <- es_run( forecast, 0.2)
#   return( forecast)#[2:length(forecast)] )
# }
