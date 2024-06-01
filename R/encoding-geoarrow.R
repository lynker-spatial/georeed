.as_geoarrow_helper <- function(x) {
  if (typeof(x) == "list") {
    x <- lapply(x, .as_geoarrow_helper)
  }

  if (is.matrix(x)) {
    setNames(
      as.data.frame(x),
      c("x", "y", "z")[seq_len(ncol(x))]
    )
  } else {
    unclass(x)
  }
}

as_geoarrow <- function(.x, ...) {
  UseMethod("as_geoarrow")
}

as_geoarrow.sf <- function(.x, ...) {
  geom_cols <- geometry_columns(.x)

  .tbl <- arrow::as_arrow_table(sf::st_drop_geometry(.x))
  for (geom_col in geom_cols) {
    .tbl[[geom_col]] <- as_geoarrow(.x[[geom_col]])
  }

  .tbl
}

as_geoarrow.sfc <- function(.x, ...) {
  arrow::Array$create(.as_geoarrow_helper(.x))
}
