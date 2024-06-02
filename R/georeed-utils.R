#' Get the names of geometry columns in an `sf` object
#' @param .data `sf`
#' @return `character(N)`
#'         Names of all geometry columns in the `sf` object.
#' @keywords internal
geometry_columns <- function(.data) {
  .x <- lapply(.data, \(i) inherits(i, "sfc"))
  names(which(.x == TRUE))
}
