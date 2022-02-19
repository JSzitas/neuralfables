

train_lstm_esn <- function( y,
                       n_nodes_typical = 100,
                       n_nodes_shortterm = 100,
                       n_nodes_longterm = 100,
                       shortterm_t = 4,
                       longterm_t = 10,
                       alpha = 0.3, # leaking rate
                       lambda = 1,
                       spectral_radius = 0.8,
                       scaler = scaler_min_max,
                       inv_scaler = scaler_inverse_min_max,
                       scaler_args = list( a = -0.8, b = 0.8 ),
                       n_diffs = NULL
) {

  transformers <- make_X_transformers( 0,
                                       NULL,
                                       0,
                                       seas_dummy = FALSE,
                                       index_dummy = FALSE,
                                       intercept = FALSE,
                                       scaler,
                                       scaler_args,
                                       inv_scaler,
                                       n_diffs)

  fitted_transformers <- transformers$train_prep(y)
  u <- fitted_transformers$y

  train_length <- length(u) - 1
  Win_typical <- matrix(stats::runif( n_nodes_typical * 2, -0.5, 0.5), n_nodes_typical)
  # initialize weights
  W <- matrix(runif(n_nodes_typical*n_nodes_typical,-0.5,0.5), n_nodes_typical)
  # compute spectral radius
  rho_w <- abs(eigen(W,only.values=TRUE)$values[1])
  W_typical <- W * spectral_radius / rho_w

  # typical reservoir
  X_typical <- matrix( 0,
               nrow = 2 + n_nodes_typical,
               ncol = train_length )
  x_typical = rep(0, n_nodes_typical)
  for (t in 1:train_length){
    u_t = u[t]
    x_typical = (1-alpha) * x_typical + alpha*tanh( Win_typical %*% rbind(1,u_t) +
                                                    W_typical %*% x_typical )
    X_typical[,t] = rbind( 1, u_t, x_typical )
  }
  # # short term memory reservoir
  # Win_shortterm <- matrix(runif( n_nodes_shortterm * 2, -0.5, 0.5), n_nodes_shortterm)
  # # initialize weights
  # W <- matrix(runif(n_nodes_shortterm*n_nodes_shortterm,-0.5,0.5), n_nodes_shortterm)
  # # compute spectral radius
  # rho_w <- abs(eigen(W,only.values=TRUE)$values[1])
  # W_shortterm <- W * spectral_radius / rho_w
  #
  # X_short <- matrix( 0,
  #                      nrow = 2 + n_nodes_shortterm,
  #                      ncol = train_length )
  # x_short = rep(0, n_nodes_shortterm)
  # for (t in 1:train_length){
  #   if( t < shortterm_t  ){
  #     u_t = runif(1)
  #   }
  #   else{
  #     u_t = u[t-shortterm_t]
  #   }
  #   x_short = (1-alpha) * x_short + alpha*tanh( Win_shortterm %*% rbind(1,u_t) + W_shortterm %*% x_short )
  #   X_short[,t] = rbind( 1, u_t, x_short )
  # }

  # long term memory reservoir
  Win_longterm <- matrix(runif( (n_nodes_longterm+2) * 2, -0.5, 0.5), n_nodes_longterm+2)
  # initialize weights
  W <- matrix(runif((n_nodes_longterm+2)*(n_nodes_longterm+2),-0.5,0.5), n_nodes_longterm+2)
  # compute spectral radius
  rho_w <- abs(eigen(W,only.values=TRUE)$values[1])
  W_longterm <- W * spectral_radius / rho_w

  X_long <- matrix( 0,
                    nrow = 2 + n_nodes_longterm,
                    ncol = train_length )
  x_long = rep(0, n_nodes_longterm)
  for (t in 1:train_length){
    u_t = u[t]
    if( t > longterm_t ) {
      new_x_long <- X_long[, t - longterm_t]
    }
    else {
      new_x_long <- runif(n_nodes_longterm+2)
    }
    # print(W_longterm %*% new_x_long)
    cat(t, "\n")
    # cat(new_x_long, "\n")
    # if( t == 1) {
    #   return(list(W_longterm, new_x_long))
    # }
    x_long = (1-alpha) * new_x_long + alpha*tanh( Win_longterm %*% rbind(1,u_t) +
                                                  W_longterm %*% new_x_long )
    X_long[,t] = rbind( 1, u_t, x_long )
  }
  # bind reservoirs together
  X <- cbind( X_long,
              X_typical#,
              # X_short
              )

  # train the output
  Wout <- u[2:length(u)] %*% t(X) %*% solve( X %*% t(X) + lambda * diag( nrow(X)) )
  fitted <- Wout %*% X
  fitted <- fitted_transformers$inverse_scaler( fitted )

  structure( list( data = y,
                   fitted = c(rep(NA,1),fitted),
                   transform_fit = fitted_transformers,
                   transformers = transformers,
                   weights = Wout,
                   x_typical = x_typical,
                   # x_short = x_short,
                   # X_short = X_short,
                   # W_short = W_shortterm,
                   # Win_shortterm= Win_shortterm,
                   x_long = x_long,
                   X_long = X_long,
                   W_typical = W_typical,
                   W_longterm = W_longterm,
                   # Win_shortterm = Win_shortterm,
                   Win_longterm = Win_longterm,
                   alpha = alpha ),
             class = "lstmESN")
}

forecast.lstmESN <- function( object, h = 8, ... ) {

  u = object$transform_fit$y
  u = u[length(u)]
  trained_transformers <- object$transform_fit

  x_typical = object$x_typical
  x_long = object$x_long
  x_short = object$x_short
  Win = object$w_in
  alpha = object$alpha
  Wout = object$weights
  W = object$W

  forecast <- rep(NA, h)
  for (step in seq_len(h) ) {
    x = (1 - alpha ) * x + alpha * tanh( Win %*% rbind( 1, u ) + W %*% x )
    forecast[step] = Wout %*% rbind(1,u,x)
    u = forecast[step]
  }
  return(trained_transformers$inverse_scaler(forecast))
}

