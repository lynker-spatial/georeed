#' @keywords internal
geometry_columns <- function(.data) {
  .x <- lapply(.data, \(i) inherits(i, "sfc"))
  names(which(.x == TRUE))
}
