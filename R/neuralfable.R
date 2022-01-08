train_neuralfable <- function(.data, specials, ...) {

  structure(
    list(
      NULL
    ),
    class = "NEURALFABLE"
  )
}

specials_neuralfable <- fabletools::new_specials(
  parameters = function( ... ) {
    rlang::dots_list( ..., named = TRUE)
  },
  xreg = function(...) {
    # This model doesn't support exogenous regressors, time to error.
    stop("Exogenous regressors aren't supported by `neuralfables` at this time.")
  },
  .required_specials = c("parameters")
)
#' Neuralfable models
#'
#' @description Neurafable model class
#' @param formula A neuralfable model formula. Ths encompasses all models exposed by neuralfables (see details).
#' @param ... Additional arguments (see details).
#' @return A specified model, analogous to other model objects within fable/fabletools.
#' @details Accepts and parses several model specials.
#' @note Maybe some other day.
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
