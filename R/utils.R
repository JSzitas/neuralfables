# check_trend_season <- function( .data ) {
#   target <- rlang::sym(tsibble::measured_vars(.data)[1])
#   mdl <- fabletools::model( .data, fable::ETS( !!target ) )
#   selected_model <- fabletools::model_sum(test[[1]][[1]][["fit"]])
#   # this looks for the trend of the fitted ETS( error, TREND, season ) model -
#   # where N means "no-trend" - in every other case, we assume there is a trend.
#   trend <- !grepl( pattern = ",N,", x = selected_model )
#   # ths looks for the seasonality of the fitted ETS(error, trend, SEASON ) model
#   # where N means, again, "no-season"
#   season <- !grepl( pattern = ",[A-za-z],N", x = selected_model )
#   return( list(trend = trend, season = season) )
# }


resolve_intercept <- function(x, p_threshold = 0.05, use_intercept = NULL) {
  if (!use_intercept) {
    return(FALSE)
  }
  if (isTRUE(use_intercept)) {
    return(TRUE)
  }
  if (stats::wilcox.test(x)$p.value <= p_threshold) {
    return(TRUE)
  }
  FALSE
}

fix_lagged_names <- function( lag_mat, col_name ) {
  colnames(lag_mat) <- paste0(col_name, "_lag_", seq_len(ncol(lag_mat)))
  return(lag_mat)
}


make_lag_matrix <- function(x, lags = 1:5) {

  lag_mat_list <- apply(as.matrix(x), 2, function(col) {
    # within lambda:
    # bind all lags for a given column
    do.call(
      cbind,
      # inner loop - create lags for each column
      purrr::map(lags, function(lag) {
        dplyr::lag(col, lag)
      })
    )
  }, simplify = FALSE)

  lag_mat_list <- purrr::imap( lag_mat_list, ~ fix_lagged_names(.x,.y) )
  do.call(
    # bind all created lag matrices together
    cbind,
    lag_mat_list
  )
}

sigmoid <- function(x) {
  y <- x / (1 + abs(x))
  return(y)
}

apply_differences <- function(y, differences = 0, lag = 0) {
  if (any(differences != 0)) {
    differences <- differences[differences > 0]
    integration_constants <- rep(NA, length(differences))
    for (d in seq_len(length(differences))) {
      integration_constants[d] <- y[1 + lag]
      y <- diff(y, differences = differences[d])
    }
  } else {
    integration_constants <- 0
  }
  return(list(
    y = y,
    differences = differences,
    integration_constants = integration_constants
  ))
}

undifference <- function(y, differences, constants) {
  if (any(differences != 0)) {
    for (d in seq_len(length(differences))) {
      y <- stats::diffinv(y, differences = differences[d], xi = constants[d])
    }
  }
  return(y)
}

get_ndiffs <- function(y, ndiffs = NULL) {
  if (!is.null(ndiffs)) {
    return(ndiffs)
  }
  ndiffs <- purrr::safely(feasts::unitroot_ndiffs, otherwise = 0)(y, alpha = 0.1)
  ndiffs <- c(
    ndiffs,
    purrr::safely(feasts::unitroot_nsdiffs, otherwise = 0)(y, alpha = 0.1)
  )
  unique(unlist(ndiffs))
}

# create a closure which can be applied to both traning and forecasting data
make_X_transformers <- function(lags = NULL,
                                xreg = NULL,
                                xreg_lags = NULL,
                                seas_dummy = TRUE,
                                index_dummy = TRUE,
                                intercept = NULL,
                                scaler = scaler_min_max,
                                scaler_args = list(a = -0.8, b = 0.8),
                                inverse_scaler = scaler_inverse_min_max,
                                n_diffs = NULL) {
  if (is.null(lags)) {
    lags <- 1:4
  }

  train_prep <- function(y) {

    # check for differences
    ndiffs <- get_ndiffs(y, n_diffs)
    differencing_result <- apply_differences(y, ndiffs, max(lags))
    y <- differencing_result$y

    # scale
    scaling_result <- do.call(scaler, c(list(y), scaler_args))
    scaled_y <- scaling_result$x

    # create matrix of lags
    X <- make_lag_matrix(scaled_y, lags = lags)

    if (seas_dummy) {
      seasonalities <- find_seasonalities(scaled_y)
      if (length(seasonalities) > 0) {
        X_seas <- trigonometric_seasonal_dummy(length(scaled_y), seasonalities)
        X <- cbind(X, X_seas)
      } else {
        seasonalities <- NULL
      }
    } else {
      seasonalities <- NULL
    }
    if (index_dummy) {
      X_index <- time_dummy(length(scaled_y))
      X <- cbind(X, X_index)
    } else {
      X_index <- NULL
    }
    # check post scaled data for intercept and add intercept as necessary
    intercept <- resolve_intercept(scaled_y, use_intercept = intercept)
    if (intercept) {
      X <- cbind(1, X)
    }
    if (!is.null(xreg)) {
      # xreg scaling might be important
      xreg <- as.matrix(xreg)
      colnames(xreg) <- paste0("xreg_", seq_len(ncol(xreg)))
      if(!is.null(xreg_lags)) {
        xreg <- make_lag_matrix(xreg, xreg_lags)
      }
      X <- cbind(X, xreg)
    }
    X <- na.omit(X)

    return(list(
      y = scaled_y[(max(lags) + 1):length(scaled_y)],
      X = X,
      lags = lags,
      seasonalities = seasonalities,
      index = X_index,
      intercept = intercept,
      xreg = xreg,
      differencing = differencing_result,
      scaler = scaler,
      scaler_args = scaler_args,
      inverse_scaler = do.call(
        purrr::partial,
        c(
          list(.f = inverse_scaler),
          scaling_result$args
        )
      )
    ))
  }

  forecast_prep <- function(y, h, train_result, xreg = NULL) {
    new_x <- matrix(rev(utils::tail(y, max(lags)))[lags], nrow = 1)

    index <- train_result$index
    seasonalities <- train_result$seasonalities
    scaler <- train_result$scaler
    scaler_args <- train_result$scaler_args
    intercept <- train_result$intercept
    train_xreg <- train_result$xreg

    if (!is.null(seasonalities)) {
      X_seas <- trigonometric_seasonal_dummy(
        n = length(y) + h,
        seas_length = seasonalities
      )
      curr_seas <- matrix(X_seas[length(y) + 1, ], nrow = 1)
      new_x <- cbind(new_x, curr_seas)
    }
    if (!is.null(train_result$index)) {
      last_index <- matrix(index[nrow(index)] + h * c(1, -1), nrow = 1)
      new_x <- cbind(new_x, last_index)
    }
    if (intercept) {
      new_x <- cbind(1, new_x)
    }
    if (!is.null(xreg)) {
      xreg <- as.matrix( rbind( train_xreg, xreg))
      # xreg <- apply(xreg, 2, function(xreg_col) {
      #   do.call(scaler, c(list(xreg_col), scaler_args))
      # }, simplify = FALSE)
      # xreg <- as.matrix(do.call(cbind, purrr::map(xreg, ~ .$x)))
      colnames(xreg) <- paste0("xreg_", seq_len(ncol(xreg)))
      if(!is.null(xreg_lags)) {
        xreg <- make_lag_matrix(xreg, xreg_lags)
      }
      xreg <- xreg[ nrow(train_xreg):(nrow(train_xreg)+h), ]
      new_x <- cbind(new_x, xreg[h, ])
    }
    return(new_x)
  }

  postprocessor <- function(y, train_result, undiff = TRUE, rescale = TRUE, add_nas = TRUE, ...) {
    diff_data <- train_result[["differencing"]]
    if (rescale) {
      y <- train_result$inverse_scaler(y)
    }
    if (undiff) {
      y <- undifference(y, diff_data[["differences"]], diff_data[["integration_constants"]])
    }
    if (add_nas) {
      total_nas <- max(diff_data[["differences"]], train_result[["lags"]])
      y <- c(rep(NA, total_nas), y)
    }
    return(y)
  }

  return(list(train_prep = train_prep, forecast_prep = forecast_prep, postprocessor = postprocessor))
}

last <- function(x) {
  x[length(x)]
}
