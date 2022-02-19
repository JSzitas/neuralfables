
period <- function(x)
{
  n <- length(x)
  spec <- stats::spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  c(.period = period)
}

find_seasonalities <- function( y, max_iter = 5, aggregator = sum, upper_limit = 500 ) {

  periods <- list()
  for( iter in seq_len(max_iter) ) {
    last_period <- period(y)
    if( last_period <= 1 || is.na(last_period) ){
      break;
    }
    periods[[iter]] <- last_period
    y <- slider::slide_dbl(y, .f = aggregator, .step = last_period )
    y <- stats::na.omit(y)
  }
  x <- cumprod( unlist(periods))
  x[ x < upper_limit ]
}

trigonometric_seasonal_dummy <- function( n, seas_length = c(12) ) {

  seas <- purrr::map( seas_length, function( season ) {

    sin_seas <- sin( 2*pi*seq_len(n)/season )
    cos_seas <- cos( 2*pi*seq_len(n)/season )

    sin_seas <- sin_seas/sum(sin_seas)
    cos_seas <- cos_seas/sum(cos_seas)

    cbind( sin_seas,
           cos_seas )
  })
  do.call(cbind, seas)
}

time_dummy <- function( length_out ) {
  cbind( seq_len(length_out),
         length_out - seq_len(length_out) + 1)
}


auto_trig_seas_dummy <- function( y, max_iter = 3, upper_limit = 1200 ) {

  seasonalities <- find_seasonalities( y, max_iter = max_iter, upper_limit = upper_limit)

  trigonometric_seasonal_dummy( length(y), seasonalities )
}

# function (n, m = NULL, y = NULL, type = c("bin", "trg"), full = c(FALSE,
#                                                                   TRUE))
# {
#   type <- match.arg(type, c("bin", "trg"))
#   full <- full[1]
#   if (!is.null(y)) {
#     if (is.null(m)) {
#       m <- frequency(y)
#     }
#     start <- start(y)
#     isdd <- length(start) == 2
#     if (isdd) {
#       start <- start[2]
#     }
#     else {
#       start <- start%%1
#     }
#   }
#   else {
#     start <- 1
#     isdd <- TRUE
#   }
#   if (start >= n) {
#     n.sim <- n + start
#   }
#   else {
#     n.sim <- n
#   }
#   if (is.null(m)) {
#     stop("Seasonality not specified.")
#   }
#   if ((m%%1) == 0) {
#     if (type == "bin") {
#       x <- matrix(rep(diag(rep(1, m)), ceiling(n.sim/m)),
#                   ncol = m, byrow = TRUE)[1:n.sim, , drop = FALSE]
#     }
#     else {
#       x <- array(0, c(n.sim, m))
#       t <- 1:n.sim
#       for (i in 1:(m/2)) {
#         x[, 1 + (i - 1) * 2] <- cos((2 * t * pi * i)/m +
#                                       (2 * pi * (start - isdd))/m)
#         x[, 2 + (i - 1) * 2] <- sin((2 * t * pi * i)/m +
#                                       (2 * pi * (start - isdd))/m)
#       }
#     }
#     if (full == FALSE) {
#       x <- x[, 1:(m - 1), drop = FALSE]
#     }
#   }
#   else {
#     x <- array(0, c(n.sim, 2))
#     t <- 1:n.sim
#     x[, 1] <- cos((2 * t * pi)/m + (2 * pi * (start - isdd)))
#     x[, 2] <- sin((2 * t * pi)/m + (2 * pi * (start - isdd)))
#   }
#   if (start > 1 & type == "bin") {
#     x <- rbind(x[start:n, , drop = FALSE], x[1:(start - 1),
#                                              , drop = FALSE])
#   }
#   x <- x[1:n, , drop = FALSE]
#   return(x)
# }
