#' Read a GeoParquet file
#'
#' '[GeoParquet](https://geoparquet.org/)' is a geospatial columnar
#' format backed by the '[Parquet](https://parquet.apache.org/)' file format.
#'
#' @inheritParams arrow::read_parquet
#' @inheritDotParams arrow::read_parquet
#'
#' @return An `sf` table if `as_data_frame` is `TRUE` (the default), or an
#'        Arrow [Table] otherwise.
#'
#' @export
read_geoparquet <- function(file,
                            col_select = NULL,
                            as_data_frame = TRUE,
                            props = arrow::ParquetArrowReaderProperties$create(),
                            mmap = TRUE,
                            ...) {

  .tbl <- arrow::read_parquet(
    file,
    col_select = col_select,
    as_data_frame = as_data_frame,
    props = props,
    mmap = mmap,
    ...
  )



  if (as_data_frame) {
    # Convert to `sf`
    
  }

  return(.tbl)
}


#' Write GeoParquet file to disk
#'
#' '[GeoParquet](https://geoparquet.org/)' is a geospatial columnar
#' format backed by the '[Parquet](https://parquet.apache.org/)' file format.
#'
#' @inheritParams arrow::write_parquet
#' @param ... Additional arguments passed to [geoparquet_metadata].
#' @param covering `logical(1)`
#'        `r lifecycle::badge("experimental")`
#'        If `TRUE`, includes a `bbox` column in `x` and adds
#'        the experimental covering metadata from the GeoParquet 1.1
#'        specification.
#' @seealso geoparquet_metadata
#' @return The input `x` invisibly
#'
#' @export
write_geoparquet <- function(x,
                             sink,
                             chunk_size = NULL,
                             version = "2.4",
                             compression = default_parquet_compression(),
                             compression_level = NULL,
                             use_dictionary = NULL,
                             write_statistics = NULL,
                             data_page_size = NULL,
                             use_deprecated_int96_timestamps = FALSE,
                             coerce_timestamps = NULL,
                             allow_truncated_timestamps = FALSE,
                             ...,
                             geometry_encoding = c("wkb", "arrow"),
                             covering = FALSE) {

  geometry_encoding <- match.arg(geometry_encoding)

  if (inherits(x, "sf")) {
    metadata <- geoparquet_metadata(
      x,
      ...,
      encoding = geometry_encoding,
      covering = covering
    )

    if (geometry_encoding == "arrow") {
      .tbl <- as_geoarrow(x)
    } else {
      .tbl <- arrow::Table$create(encode_wkb(x))
    }

    if (covering) {
      .tbl$bbox <- arrow::StructArray$create(
        as.data.frame(do.call(rbind, lapply(sf::st_geometry(x), sf::st_bbox)))
      )
    }

    .tbl$metadata <- c(.tbl$metadata, metadata)
  } else {
    .tbl <- arrow::Table$create(x)
  }

  arrow::write_parquet(
    x = .tbl,
    sink = sink,
    chunk_size = chunk_size,
    version = version,
    compression = compression,
    compression_level = compression_level,
    use_dictionary = use_dictionary,
    write_statistics = write_statistics,
    data_page_size = data_page_size,
    use_deprecated_int96_timestamps = use_deprecated_int96_timestamps,
    coerce_timestamps = coerce_timestamps,
    allow_truncated_timestamps = allow_truncated_timestamps
  )

  invisible(x)
}


#' @keywords internal
default_parquet_compression <- function() {
  if (arrow::codec_is_available("snappy")) {
    return("snappy")
  } else {
    return(NULL)
  }
}
