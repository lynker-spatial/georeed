#' Read a GeoParquet file
#'
#' '[GeoParquet](https://geoparquet.org/)' is a geospatial columnar
#' format backed by the '[Parquet](https://parquet.apache.org/)' file format.
#'
#' @inheritParams arrow::read_parquet
#' @inheritDotParams arrow::read_parquet
#'
#' @return An `sf` table if `as_data_frame` is `TRUE` (the default), or an
#'        `arrow::Table` otherwise.
#' @export
read_geoparquet <- function(file,
                            col_select = NULL,
                            as_data_frame = TRUE,
                            props = arrow::ParquetArrowReaderProperties$create(),
                            mmap = TRUE,
                            ...) {

  .args <- list(...)
  .args$file <- file
  .args$col_select <- col_select
  .args$as_data_frame <- FALSE
  .args$props <- props
  .args$mmap <- mmap

  .tbl <- do.call(arrow::read_parquet, .args)

  if (as_data_frame) {
    .meta <- .tbl$metadata
    .tbl <- dplyr::collect(.tbl)

    if ("geo" %in% names(.meta)) {
      # Convert to `sf`
      geo <- jsonlite::fromJSON(.meta$geo)

      for (nm in names(geo$columns)) {
        col <- geo$columns[[nm]]
        if (col$encoding == "wkb") {
          .tbl[[nm]] <- sf::st_as_sfc(
            x = .tbl[[nm]],
            crs = sf::st_crs(col$crs)
          )

          # TODO: Assert col$geometry_types?
          # TODO: Assert col$orientation?
        } else {
          # encoding is geoarrow, or unsupported
          # FIXME: implement geoarrow reading,
          #        needs: handle supported types,
          #               probably in separate functions
          warning("Reading non-WKB GeoParquet is currently unsupported.\n",
                  "Returning as if `arrow::read_parquet` was called.",
                  call. = FALSE)
        }
      }

    }
  }

  return(.tbl)
}


#' Write GeoParquet file to disk
#'
#' '[GeoParquet](https://geoparquet.org/)' is a geospatial columnar
#' format backed by the '[Parquet](https://parquet.apache.org/)' file format.
#'
#' @inheritParams arrow::write_parquet
#' @param x `sf`, `data.frame`, `arrow::RecordBatch`, or `arrow::Table`
#' @param ... Additional arguments passed to [geoparquet_metadata].
#' @param geo_encoding \emph{\code{character(1)}} `r lifecycle::badge('experimental')`\cr
#' Either `wkb` or `arrow`. If `wkb`, geometry is encoded in WKB
#' If `arrow`, then the geometry is encoded in GeoArrow format.
#' @param geo_covering \emph{\code{logical(1)}} `r lifecycle::badge('experimental')`\cr
#' If `TRUE`, includes a `bbox` column in `x` and adds
#' the experimental covering metadata from the GeoParquet 1.1
#' specification.
#' @return The input `x` invisibly
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
                             geo_encoding = c("wkb", "arrow"),
                             geo_covering = FALSE) {

  geo_encoding <- match.arg(geo_encoding)

  if (inherits(x, "sf")) {
    metadata <- geoparquet_metadata(
      x,
      ...,
      encoding = geo_encoding,
      covering = geo_covering
    )

    if (geo_encoding == "arrow") {
      .tbl <- as_geoarrow(x)
    } else {
      .tbl <- as_wkbarrow(x)
    }

    if (geo_covering) {
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
