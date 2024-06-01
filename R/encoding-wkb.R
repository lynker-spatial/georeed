as_wkbarrow <- function(.x, ...) {
  UseMethod("as_wkbarrow")
}

as_wkbarrow.sf <- function(.x, ...) {
  geom_cols <- geometry_columns(.x)

  .tbl <- arrow::as_arrow_table(sf::st_drop_geometry(.x))
  for (geom_col in geom_cols) {
    .tbl[[geom_col]] <- as_wkbarrow(.tbl[[geom_col]])
  }

  .tbl
}

as_wkbarrow.sfc <- function(.x, ...) {
  arrow::Array$create(
    unclass(sf::st_as_binary(.x)),
    type = arrow::binary()
  )
}
