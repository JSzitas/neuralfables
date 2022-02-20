#' @importFrom stats residuals
#' @export
residuals.NEURALFABLE <- function(object, ...) {
  object[["resid"]]
}
#' @importFrom stats fitted
#' @export
fitted.NEURALFABLE <- function(object, ...) {
  object[["fitted"]]
}
#' @importFrom generics generate
#' @export
generate.NEURALFABLE <- function(object, new_data = NULL, specials = NULL, bootstrap = FALSE, ...) {
  h <- nrow(new_data)
  res <- residuals(object)

  sample(na.omit(res) - mean(res, na.rm = TRUE), h, replace = TRUE)
}
#' @importFrom fabletools forecast
#' @export
forecast.NEURALFABLE <- function(object, new_data = NULL, specials = NULL, bootstrap = FALSE,
                                 times = 50, ...) {
  h <- nrow(new_data)
  mean_fcst <- forecast(object[["fit"]], h = h)
  if (bootstrap) {
    generated <- purrr::map(
      seq_len(times),
      ~ generate(object, new_data, specials, bootstrap, ...) +
        mean_fcst
    )
    return(distributional::dist_sample(generated))
  }

  res <- residuals(object)
  distributional::dist_normal(mean_fcst, sqrt(stats::var(res, na.rm = TRUE)))
}
#' @importFrom generics refit
#' @export
refit.NEURALFABLE <- function(object, new_data, specials = NULL,
                              ...) {
  y <- unclass(new_data)[[tsibble::measured_vars(new_data)]]
  parameters <- object$parameters
  trainer <- object$method
  xreg <- specials$xreg[[1]]$xreg

  model <- do.call(trainer, c(list(y = y), parameters, xreg = xreg))
  structure(
    list(
      fit = model,
      method = trainer,
      parameters = parameters,
      resid = model[["resid"]],
      fitted = model[["fitted"]]
    ),
    class = "NEURALFABLE"
  )
}
