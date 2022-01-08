


ESN <- function( y,
                 input_size = 1,
                 # out_size = 1,
                 lags = 1:5,
                 seas_dummy = TRUE,
                 n_nodes = 100,
                 # resSize = 1000
                 alpha = 0.3, # leaking rate
                 lambda = 1,
                 init_length = 0,
                 train_length = NULL
                 ) {

  if(is.null(train_length)) {
    train_length <- length(y) - 1
  }

  Win <- matrix(runif( n_nodes * ( 1 + input_size), -0.5, 0.5), n_nodes)
  # initialize weights
  W <- matrix(runif(n_nodes*n_nodes,-0.5,0.5),n_nodes)

  # compute spectral radius
  rho_w <- abs(eigen(W,only.values=TRUE)$values[1])
  W <- W * 1.25 / rho_w

  X <- matrix( 0,
              nrow = 1 + input_size + n_nodes,
              ncol = train_length ) # - init_length )
              # set the corresponding target matrix directly
  Yt <- matrix(y[(2#init_length+2
                  ):(train_length+1)],1)

  # run the reservoir with the data and collect X
  x = rep(0, n_nodes)
  for (t in 1:train_length){
    u = y[t]
    x = (1-alpha) * x + alpha*tanh( Win %*% rbind(1,u) + W %*% x )
    if (t > init_length)
      X[,t #- init_length
        ] = rbind( 1, u, x )
  }
  # train the output
  # replace with the cholesky decomposition please, this hurts my brain
  Wout <- Yt %*% t(X) %*% solve( X %*% t(X) + lambda * diag( 1 + input_size + n_nodes) )
  structure( list( y_t = Yt,
                   weights = Wout,
                   data = y,
                   x = x,
                   W = W,
                   w_in = Win,
                   alpha = alpha,
                   out_size = 1),
             class = "ESN")
}

forecast.ESN <- function( object, h = 8, ... ) {

  u = object$data[length(object$data)]
  out_size = 1
  x = object$x
  Win = object$w_in
  alpha = object$alpha
  Wout = object$weights
  W = object$W

  Y <- rep(0, h)
  # u = data[train_length+1]
  for (t in seq_len(h) ) {
    x = (1 - alpha ) * x + alpha * tanh( Win %*% rbind( 1, u ) + W %*% x )
    y = Wout %*% rbind(1,u,x)
    Y[t] = y
    # generative mode:
    u = y
    ## this would be a predictive mode:
    #u = data[train_length+t+1]
  }
  return(Y)
}
