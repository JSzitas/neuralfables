
poisson_ridge_wls <- function(y, X, reg_lambda = 1e-8, iter_max = 200, tol = 1e-8) {

  estimate_lambda <- FALSE
  # if null, find coefficients for lambda == 0 first
  if( is.null(reg_lambda) ) {
    estimate_lambda <- TRUE
    reg_lambda <- 0
    ml_coef <- poisson_ridge_wls( y, X, reg_lambda = 0, iter_max, tol )[["coef"]]
  }

  u <- y + stats::runif(length(y))
  W <- diag(1, nrow = nrow(X))
  z <- log(u) + ((y - u) / u)
  reg_lambda <- diag(reg_lambda, nrow = ncol(X))

  coef <- rep(0, ncol(X))

  loss <- 0
  iter <- 1
  while (TRUE) {
    if (iter > iter_max) {
      break
    }
    # compute ths ahead of time
    tXWX <- t(X) %*% W %*% X

    # this allows us to compute the ridge lambda parameter, using a whack estimator from
    # "Poisson Ridge Regression Estimators: A Performance Test" (2021),
    # known in that paper as "k36" (k37 looks potentially promising but, adding a potentially confusing choice
    # is not something I am too keen on, as it is unclear exactly when which estimator is better - and it is
    # used here primarily to avoid requiring more confusing choices from the user. )
    if( estimate_lambda ) {
      # the weighed covariance matrix is symmetric
      tXWX_eigen <- eigen( tXWX, symmetric = TRUE )
      lambdas <- tXWX_eigen[["values"]]
      # find an estimate of alpha and multiply by coefficients found for unpenalized regression
      alpha <- ml_coef %*% tXWX_eigen[["vectors"]]
      n <- nrow(X)
      p <- ncol(X)

      reg_lambda <- stats::median( lambdas/((n-p) + (lambdas* (alpha^2))))
    }

    new_coef <- solve(tXWX + reg_lambda) %*% t(X) %*% W %*% z
    u <- c(exp(X %*% new_coef))
    z <- log(u) + ((y - u) / u)
    W <- diag( u )

    loss_new <- sum(u - y * log(u))
    if (abs(loss_new - loss) < tol) {
      break
    }
    loss <- loss_new
    coef <- new_coef
    iter <- iter + 1
  }
  return(structure( list(coef = c(coef)),
                    class = "poisson_glm"
                    )
  )
}
#' @importFrom stats predict
predict.poisson_glm <- function(x, new_data, ...) {
  c(exp(new_data %*% x[["coef"]]))
}



#
# logit_link <- function( x ) {
#   y <- c(1/(1+exp(-x)))
#   y[ y < 0.01 ] <- 0.01
#   y[ y > 0.99 ] <- 0.99
#   return(y)
# }
#
# binomial_ridge_wls <- function(y, X, reg_lambda = 1e-8, iter_max = 300, tol = 1e-4) {
#   u <- logit_link( X %*% stats::runif(ncol(X)))
#   W <- diag(1, nrow = nrow(X))
#   z <- (y - u) / ( u * (1-u))
#   reg_lambda <- diag(reg_lambda, nrow = ncol(X))
#
#   coef <- rep(0, ncol(X))
#
#   loss <- 0
#   iter <- 1
#   while (TRUE) {
#     if (iter > iter_max) {
#       break
#     }
#
#     new_coef <- solve(t(X) %*% W %*% X + reg_lambda) %*% t(X) %*% W %*% z
#     u <- logit_link( X %*% new_coef )
#     z <- (y - u) / ( u * (1-u))
#     W <- diag(u)
#
#     loss_new <- sum( -y*log(u) -(1-y)*log(u) )
#     if (abs(loss_new - loss) < tol | all(abs(new_coef - coef) < 0.1) ) {
#       break
#     }
#     loss <- loss_new
#     coef <- new_coef
#     iter <- iter + 1
#   }
#   return(structure( list(coef = c(coef)),
#                     class = "logistic_glm"
#   ))
# }
#
# predict.logistic_glm <- function(x, new_data, threshold = 0.5, ...) {
#   as.integer( logit_link(new_data %*% x[["coef"]]) > threshold )
# }


# this does not require IWLS :)
gaussian_ridge_ls <- function( y, X, reg_lambda = 1e-8, ... ) {

  tXX <- t(X) %*% X
  # if null, find coefficients for lambda == 0 first
  if( is.null(reg_lambda) ) {
    ml_coef <- gaussian_ridge_ls( y, X, reg_lambda = 0, ... )[["coef"]]
    # the weighed covariance matrix is symmetric
    tXX_eigen <- eigen( tXX, symmetric = TRUE )
    lambdas <- tXX_eigen[["values"]]
    # find an estimate of alpha and multiply by coefficients found for unpenalized regression
    alpha <- ml_coef %*% tXX_eigen[["vectors"]]
    n <- nrow(X)
    p <- ncol(X)

    reg_lambda <- stats::median( lambdas/((n-p) + (lambdas* (alpha^2))))
  }

  coef <- y %*% X %*% solve(tXX + reg_lambda * diag(ncol(X)))

  return(structure( list(coef = c(coef),
                         lambda = reg_lambda),
                    class = "gaussian_glm" )
         )
}

predict.gaussian_glm <- function( x, new_data, ... ) {
  c(new_data %*% x[["coef"]])
}
#
# # a poisson-binomial hurdle model
# poisson_bin_hurdle_iwls <- function(y, X, reg_lambda = 1e-8, iter_max = 200, tol = 1e-8,... ) {
#
#   # hurdle - a model on whether something is zero
#   is_zero <- binomial_ridge_wls( y = y,
#                                  X = X,
#                                  reg_lambda = reg_lambda,
#                                  iter_max = iter_max,
#                                  tol = tol,
#                                  ...)
#
#   # counts - only the nonzero parts
#   nonzero_indices <- which( y > 0 )
#   counts <- poisson_ridge_wls( y = y[nonzero_indices],
#                                X = X[nonzero_indices,],
#                                reg_lambda = reg_lambda,
#                                iter_max = iter_max,
#                                tol = tol,
#                                ... )
#   structure( list( binomial = is_zero,
#                    poisson = counts ),
#              class = "inflation_model")
# }
#
# predict.inflation_model <- function(x, new_data, ...) {
#
#   preds <- purrr::map( x, function(model){
#     predict(model, new_data, ...)
#   })
#   purrr::reduce( preds, `*` )
# }
#
# poisson_bin_zero_inflated_iwls <- function(y, X, reg_lambda = 1e-8, iter_max = 200, tol = 1e-8,...) {
#
#   # a model on the 'first kind' of zero
#   is_zero <- binomial_ridge_wls( y = y,
#                                  X = X,
#                                  reg_lambda = reg_lambda,
#                                  iter_max = iter_max,
#                                  tol = tol,
#                                  ...)
#   # counts - both zero and non-zero parts - this has the 'second kind' of zero
#   counts <- poisson_ridge_wls( y = y,
#                                X = X,
#                                reg_lambda = reg_lambda,
#                                iter_max = iter_max,
#                                tol = tol,
#                                ... )
#   structure( list( binomial = is_zero,
#                    poisson = counts ),
#              class = "inflation_model")
# }

ridge_solver <- function( y, X, lambda = 1e-8, family = c("gaussian","poisson","hurdle","zero-inflated"),... ) {

  if( length(family) > 1) {
    family <- family[1]
    rlang::warn( glue::glue("Multiple arguments provided to 'family' - using the first one, '{family}'." ) )
  }

  if( !(family %in% c("gaussian","poisson","hurdle","zero-inflated"))) {
    rlang::abort( glue::glue( "Family {family} not supported." ) )
  }

  solver <- list( gaussian = gaussian_ridge_ls,
                  poisson = poisson_ridge_wls)[[family]]
  do.call(solver, c(list( y = y, X = X, reg_lambda = lambda), list(...) ))
}
