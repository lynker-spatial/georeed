as_arrow_binary <- function(.vec) {
  arrow::Array$create(unclass(.vec), type = arrow::binary())$as_vector()
}

st_as_arrow_binary <- function(.sfc) {
  as_arrow_binary(sf::st_as_binary(.sfc))
}

encode_wkb <- function(.data) {
  geom_columns <- geometry_columns(.data)
  .data <- as.data.frame(.data)

  for (col in geom_columns) {
    .data[[col]] <- st_as_arrow_binary(.data[[col]])
  }

  .data
}
