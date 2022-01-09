


train_esn <- function( y,
                       n_nodes = 100,
                       alpha = 0.3, # leaking rate
                       lambda = 1,
                       input_scale = 0.5,
                       activation = tanh,
                       spectral_radius = 0.8,
                       scaler = scaler_min_max,
                       inv_scaler = scaler_inverse_min_max,
                       scaler_args = list( a = -0.8, b = 0.8 ),
                       n_diffs = NULL
                 ) {

  transformers <- make_X_transformers( lags = 0,
                                       xreg = NULL,
                                       xreg_lags = NULL,
                                       seas_dummy = FALSE,
                                       index_dummy = FALSE,
                                       intercept = FALSE,
                                       scaler = scaler,
                                       scaler_args = scaler_args,
                                       inverse_scaler = inv_scaler,
                                       n_diffs = n_diffs)

  fitted_transformers <- transformers$train_prep(y)
  u <- fitted_transformers$y

  train_length <- length(u) - 1
  Win <- matrix(runif( n_nodes * 2, -input_scale, input_scale), n_nodes)
  # initialize weights
  W <- matrix(runif(n_nodes*n_nodes, -input_scale, input_scale), n_nodes)
  # compute spectral radius
  rho_w <- abs(eigen(W,only.values=TRUE)$values[1])
  W <- W * spectral_radius / rho_w

  # run the reservoir with the data and collect X
  X <- matrix( 0,
               nrow = 2 + n_nodes,
               ncol = train_length )
  x = rep(0, n_nodes)
  for (t in 1:train_length){
    u_t = u[t]
    x = (1-alpha) * x + alpha*activation( Win %*% rbind(1,u_t) + W %*% x )
    X[,t] = rbind( 1, u_t, x )
  }

  # train the output
  Wout <- u[2:length(u)] %*% t(X) %*% solve( X %*% t(X) + lambda * diag( nrow(X)) )
  fitted <- Wout %*% X
  fitted <- fitted_transformers$inverse_scaler( fitted )

  structure( list( data = y,
                   fitted = c(rep(NA,1),fitted),
                   transform_fit = fitted_transformers,
                   transformers = transformers,
                   weights = Wout,
                   activation = activation,
                   x = x,
                   W = W,
                   w_in = Win,
                   alpha = alpha ),
             class = "ESN")
}

forecast.ESN <- function( object, h = 8, ... ) {

  u = object$transform_fit$y
  u = u[length(u)]
  trained_transformers <- object$transform_fit

  x = object$x
  Win = object$w_in
  alpha = object$alpha
  Wout = object$weights
  W = object$W
  activation = object$activation

  forecast <- rep(NA, h)
  for (step in seq_len(h) ) {
    x = (1 - alpha ) * x + alpha * activation( Win %*% rbind( 1, u ) + W %*% x )
    forecast[step] = Wout %*% rbind(1,u,x)
    u = forecast[step]
  }
  return(trained_transformers$inverse_scaler(forecast))
}
