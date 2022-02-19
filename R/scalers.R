
scaler_min_max <- function( x, a = -0.8, b = 0.8  ) {

  min_x = min(x, na.rm = TRUE)
  max_x = max(x, na.rm = TRUE)

  list( x = ( b - a ) * ( x - min_x )/( max_x - min_x ) + a,
        args = list( a = a,
                     b = b,
                     min_x = min_x,
                     max_x = max_x ))
}

scaler_inverse_min_max <- function( x, min_x = 0, max_x = 1, a = -1, b = 1  ) {
  # (max_x - min_x )*(x - min(x))/( max(x) - min(x)) + min_x
  ( max_x - min_x )*( x - a )/( b - a ) + min_x
}

scaler_identity <- function( x ) {
  list(x = x)
}

scaler_inverse_identity <- function( x ) {
  x
}
