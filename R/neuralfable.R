train_neuralfable <- function(.data, specials, ...) {

  # dispatch onto a training method - specials must be able to parse everything so that we can format.
  # possibly add preprocessing here (rather than within individual methods?)
  parameters <- specials$parameters[[1]]
  y <- c(.data)[[tsibble::measured_vars(.data)]]
  trainer <- specials$method[[1]]

  model <- do.call( trainer, c( list( y = y), parameters ))
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
  method = function( method = c("mlp","garbage","elm","esn") ) {
    if( length(method) > 1) {
      method <- method[1]
      rlang::warn( glue::glue("Multiple arguments provided to 'method' - using the first one, '{method}'." ) )
    }

    if( !(method %in% c("mlp","garbage","elm","esn"))) {
      rlang::abort( glue::glue( "method {method} not supported." ) )
    }

    trainer <- list( esn = train_esn,
                     mlp = train_mlp,
                     elm = train_elm,
                     garbage = train_garbage)[[method]]

    return(trainer)
  },
  xreg = function(...) {
    # This model doesn't support exogenous regressors, time to error.
    stop("Exogenous regressors aren't supported by `neuralfables` at this time.")
  },
  .required_specials = c("parameters", "method")
)
#' Neuralfable models
#'
#' @description Neurafable model class
#' @param formula A neuralfable model formula. This encompasses all models exposed by neuralfables (see details).
#' @param ... Additional arguments (see details).
#' @return A specified model, analogous to other model objects within fable/fabletools.
#' @details Use the method special to specify a method - available methods are **"elm", "mlp", "garbage", "esn"**,
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
