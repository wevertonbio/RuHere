ensure_counter_clockwise <- function(coords) {
  # coords: numeric matrix with x (column 1) and y (column 2)
  x <- coords[, 1]
  y <- coords[, 2]
  n <- length(x)

  # Calculate signed area using the Shoelace formula (surveyor's formula)
  signed_area <- sum(x[1:(n - 1)] * y[2:n] - x[2:n] * y[1:(n - 1)]) / 2

  # In a standard Cartesian plane:
  # - If signed_area < 0, vertices are oriented clockwise (CW).
  # - Reverse row indices to convert the ring to counter-clockwise (CCW).
  if (signed_area < 0) {
    coords <- coords[n:1, ]
  }

  return(coords)
}
