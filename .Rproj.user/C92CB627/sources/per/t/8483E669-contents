
orf_features <- function( n_garbage, scaling = 0.1){

  d <- n_garbage
  # custom cpp function
  Q <- qr.Q( qr(#QR(
    matrix(rnorm(n = d^2), nrow = d) )
  )

  # this is the chi, not the chi squared (I misread and this was a giant
  # source of a headache)
  S <- sqrt( rchisq(
    n = d,
    df = d
  ))

  W <- scaling * S * Q
  return(W)
}

# D = ncol(X)
# d = the number of garbage features

orf <- function( X, d, scaling = 0.1, full = TRUE )
{

  D <- ncol(X)

  if( D > d )
  {
    repetitions <- ceiling(D/d) - 1
    W <- orf_features( d, scaling = scaling )
    while( repetitions > 0 )
    {
      W <- rbind(W, orf_features(d, scaling = scaling))
      repetitions <- repetitions - 1
    }
    W <- W[1:D,]
  }
  else
  {
    # D < d
    W <- orf_features(d, scaling = scaling)
    W <- W[1:D,]
  }
  return(W)
  Z <- X %*% W

  if (full) {
    Z <- cbind(cos(Z), sin(Z))
  } else {
    Z <- apply( Z,MARGIN = 2,
                FUN = function(i)
                {
                  return( i + (runif(1)*2 * pi))
                })
    Z <- cos( Z )
  }
  return(Z)
}

rff <- function(X, d, scaling = 0.1, full = TRUE) # scaling also known as gamma sometimes
{
  W <- scaling * matrix(rnorm(n = d * ncol(X)), nrow = ncol(X))
  return(W)
  Z <- X %*% W
  if (full) {
    Z <- cbind(cos(Z), sin(Z))
  } else {
    Z <- apply( Z,MARGIN = 2,
                FUN = function(i)
                {
                  return( i + (runif(1)*2 * pi))
                })
    Z <- cos( Z )
  }

  return(Z)
}



garbage <- function( y,
                     n_garbage = 20,
                     lags = 1:5,
                     seas_dummy = TRUE,
                     index_dummy = TRUE,
                     lambda = 1) {

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

  W <- rff(X, d = n_garbage, scaling = 0.5 )
  Z <- X %*% W
  Z <- cbind(cos(Z), sin(Z))

  coef <- y_net %*% Z  %*% solve( t(Z) %*% Z + lambda*diag(ncol(Z)) )
  preds <- coef %*% t(Z)

  structure( list(
    data = y_,
    scale_args = y,
    lags = lags,
    coef = coef,
    fitted = inverse_min_max( preds, y$min_x, y$max_x, y$a, y$b ),
    seasonalities = seasonalities,
    index_dummy = X_index,
    W = W
    ), class = "garbage")
}

forecast.garbage <- function( object, h = 8, ...  ) {

  y <- object$data
  min_x <- object$scale_args$min_x
  max_x <- object$scale_args$max_x
  scale_a <- object$scale_args$a
  scale_b <- object$scale_args$b
  lags <- object$lags
  coef <- object$coef
  seasonalities <- object$seasonalities
  X_index <- object$index_dummy
  W <- object$W

  if( !is.null(seasonalities) ) {
    X_seas <- trigonometric_seasonal_dummy( n = length(y) + h,
                                            seas_length = seasonalities)
  }
  last_index <- X_index[nrow(X_index),]

  forecast <- rep(NA, h)
  for( step in seq_len(h)) {

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
    Z <- new_x %*% W
    Z <- cbind(cos(Z), sin(Z))
    forecast[step] <- coef %*% t(Z)
  }
  forecast <- inverse_min_max(forecast, min_x, max_x, scale_a, scale_b)

  return( forecast )
}


