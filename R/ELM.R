
ELM <- function( y,
                 lags = 1:5,
                 seas_dummy = TRUE,
                 index_dummy = TRUE,
                 intercept = FALSE,
                 n_hidden = 5,
                 n_diffs = NULL,
                 activation = sigmoid ) {
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
  y_net = y_[ (max(lags)+1) : length(y_) ]
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
  B <- MASS::ginv(H) %*% y_net

  fitted <- H %*% B

  structure( list(
    data = y_,
    scale_args = y,
    lags = lags,
    W = W,
    B = B,
    activation = activation,
    fitted = inverse_min_max( fitted, y$min_x, y$max_x, y$a, y$b ),
    intercept = intercept,
    seasonalities = seasonalities,
    index_dummy = X_index
  ), class = "ELM")
}

forecast.ELM <- function( object, h = 8, ...  ) {

  y <- object$data
  min_x <- object$scale_args$min_x
  max_x <- object$scale_args$max_x
  scale_a <- object$scale_args$a
  scale_b <- object$scale_args$b
  lags <- object$lags
  seasonalities <- object$seasonalities
  X_index <- object$index_dummy
  intercept <- object$intercept
  B <- object$B
  W <- object$W
  activation <- object$activation

  if( !is.null(seasonalities) ) {
    X_seas <- trigonometric_seasonal_dummy( n = length(y) + h,
                                            seas_length = seasonalities)
  }
  last_index <- X_index[nrow(X_index),]

  forecast <- rep(NA, h)
  for( step in seq_len(h)) {

    # new_x <- purrr::map( lags, function(lag){
    #   dplyr::lag( c(y, NA), lag)
    # })
    # new_x <- do.call( cbind, new_x )
    # new_x <- matrix( new_x[nrow(new_x),], nrow = 1)
    y_ <- c(y, forecast[seq_len(step-1)])
    new_x <- matrix( rev(tail( y_, max(lags) ))[lags], nrow = 1)
    if( !is.null(seasonalities) ) {
      curr_seas <- matrix( X_seas[ length(y) + 1,], nrow = 1)
      new_x <- cbind( new_x, curr_seas )
    }
    if( !is.null(X_index) ) {
      last_index <- matrix( last_index + c( 1, -1 ), nrow = 1)
      new_x <- cbind( new_x, last_index )
    }
    if( intercept ) {
      new_x <- cbind(1, new_x)
    }
    H <- activation(  new_x %*% W )
    forecast[step] <- c(H %*% B)
  }
  forecast <- inverse_min_max(forecast, min_x, max_x, scale_a, scale_b)

  return( forecast )
}
