train_neuralfable <- function(.data, specials, ...) {

  # dispatch onto a training method - specials must be able to parse everything so that we can format.
  # possibly add preprocessing here (rather than within individual methods?)
  parameters <- specials$parameters[[1]]
  y <- c(.data)[[tsibble::measured_vars(.data)]]
  trainer <- specials$method[[1]]
  xreg <- specials$xreg[[1]]$xreg

  model <- do.call( trainer, c( list( y = y), parameters, xreg = xreg ))
  # returns a trained model - do we want to dispatch on the class within some
  # 'forecast.neuralfable' function? (it could be a decent way to wrap this for future use)

  structure(
    list(
      fit = model,
      resid = model[["resid"]],
      fitted = model[["fitted"]]
    ),
    class = "NEURALFABLE"
  )
}

specials_neuralfable <- fabletools::new_specials(
  parameters = function( ... ) {
    list( ...)
  },
  method = function( method = c("mlp","garbage","elm","gbm") ) {
    if( length(method) > 1) {
      method <- method[1]
      rlang::warn( glue::glue("Multiple arguments provided to 'method' - using the first one, '{method}'." ) )
    }

    if( !(method %in% c("mlp","garbage","elm","gbm"))) {
      rlang::abort( glue::glue( "method {method} not supported." ) )
    }

    trainer <- list( mlp = train_mlp,
                     elm = train_elm,
                     garbage = train_garbage,
                     gbm = train_gbm)[[method]]

    return(trainer)
  },
  xreg = function(...) {
    dots <- rlang::enexprs(...)
    env <- purrr::map(rlang::enquos(...), rlang::get_env)
    env[ purrr::map_lgl(env,
                        purrr::compose( rlang::is_empty,
                                        rlang::env_parents )
                        )
         ] <- NULL
    env <- if (!rlang::is_empty(env))
      rlang::get_env(env[[1]])
    else base_env()
    constants <- purrr::map_lgl(dots, inherits, "numeric")
    constant_given <- any(purrr::map_lgl(dots[constants], `%in%`, -1:1))
    model_formula <- rlang::new_formula( lhs = NULL,
                                         rhs = purrr::reduce( dots,
                                                              function(.x, .y) rlang::call2("+", .x, .y)
                                                              )
                                         )
    xreg <- stats::model.frame( model_formula,
                                data = env,
                                na.action = stats::na.pass
                                )
    list( xreg = if (NCOL(xreg) == 0) NULL else as.matrix(xreg))
  },
  .required_specials = c("parameters", "method")
)
#' Neuralfable models
#'
#' @description Neurafable model class
#' @param formula A neuralfable model formula. This encompasses all models exposed by neuralfables (see details).
#' @param ... Additional arguments (see details).
#' @return A specified model, analogous to other model objects within fable/fabletools.
#' @details Use the method special to specify a method - available methods are **"elm", "mlp", "garbage", "gbm"**,
#' with default being **"mlp"**.
#'
#' @export
neuralfable <- function(formula, ...) {
  # Create a model class which combines the training method, specials, and data checks
  model_neuralfable <- fabletools::new_model_class("NEURALFABLE",
                                            # The training method (more on this later)
                                            train = train_neuralfable,
                                            # The formula specials (the next section)
                                            specials = specials_neuralfable,
                                            # Any checks of the unprocessed data, like gaps, ordered, regular, etc.
                                            check = function(.data) {
                                              if (!tsibble::is_regular(.data)) stop("Data must be regular")
                                            }
  )
  # Return a model definition which stores the user's model specification
  fabletools::new_model_definition(model_neuralfable, !!rlang::enquo(formula), ...)
}
