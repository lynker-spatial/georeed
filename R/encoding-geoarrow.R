#' Coerce object into a GeoArrow array
#'
#' @param .x `sf` or `sfc` object
#' @param ... Unused
#' @return An [arrow::Table] or [arrow::Array]
#' @rdname as_geoarrow
#'
#' @export
as_geoarrow <- function(.x, ...) {
  UseMethod("as_geoarrow")
}

#' @rdname as_geoarrow
#' @method as_geoarrow sf
#' @export
as_geoarrow.sf <- function(.x, ...) {
  geom_cols <- geometry_columns(.x)

  .tbl <- arrow::as_arrow_table(sf::st_drop_geometry(.x))
  for (geom_col in geom_cols) {
    .tbl[[geom_col]] <- as_geoarrow(.x[[geom_col]])
  }

  .tbl
}

#' @rdname as_geoarrow
#' @method as_geoarrow sfc
#' @export
as_geoarrow.sfc <- function(.x, ...) {
  arrow::Array$create(.as_geoarrow_helper(.x))
}

#' Recursive helper for geoarrow coercion.
#'
#' If `x` is a `list`, then this function recursively
#' applies itself to its elements. If `x` is a matrix,
#' we assume its a coordinate matrix based on `sf`'s
#' geometry structure, and convert it to a `data.frame`.
#' Otherwise, if `x` is neither of those, then we simply
#' return it unclassed.
#'
#' @keywords internal
.as_geoarrow_helper <- function(x) {
  if (is.matrix(x)) {
    setNames(as.data.frame(x), c("x", "y", "z")[seq_len(ncol(x))])
  } else {
    if (typeof(x) == "list") {
      x <- lapply(x, .as_geoarrow_helper)
    }

    unclass(x)
  }
}
