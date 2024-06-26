% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/georeed-parquet.R
\name{write_geoparquet}
\alias{write_geoparquet}
\title{Write GeoParquet file to disk}
\usage{
write_geoparquet(
  x,
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
  geo_covering = FALSE
)
}
\arguments{
\item{x}{\code{sf}, \code{data.frame}, \code{arrow::RecordBatch}, or \code{arrow::Table}}

\item{sink}{A string file path, URI, or \link[arrow]{OutputStream}, or path in a file
system (\code{SubTreeFileSystem})}

\item{chunk_size}{how many rows of data to write to disk at once. This
directly corresponds to how many rows will be in each row group in
parquet. If \code{NULL}, a best guess will be made for optimal size (based on
the number of columns and number of rows), though if the data has fewer
than 250 million cells (rows x cols), then the total number of rows is
used.}

\item{version}{parquet version: "1.0", "2.0" (deprecated), "2.4" (default),
"2.6", or "latest" (currently equivalent to 2.6). Numeric values are
coerced to character.}

\item{compression}{compression algorithm. Default "snappy". See details.}

\item{compression_level}{compression level. Meaning depends on compression
algorithm}

\item{use_dictionary}{logical: use dictionary encoding? Default \code{TRUE}}

\item{write_statistics}{logical: include statistics? Default \code{TRUE}}

\item{data_page_size}{Set a target threshold for the approximate encoded
size of data pages within a column chunk (in bytes). Default 1 MiB.}

\item{use_deprecated_int96_timestamps}{logical: write timestamps to INT96
Parquet format, which has been deprecated? Default \code{FALSE}.}

\item{coerce_timestamps}{Cast timestamps a particular resolution. Can be
\code{NULL}, "ms" or "us". Default \code{NULL} (no casting)}

\item{allow_truncated_timestamps}{logical: Allow loss of data when coercing
timestamps to a particular resolution. E.g. if microsecond or nanosecond
data is lost when coercing to "ms", do not raise an exception. Default
\code{FALSE}.}

\item{...}{Additional arguments passed to \link{geoparquet_metadata}.}

\item{geo_encoding}{\emph{\code{character(1)}} \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}\cr
Either \code{wkb} or \code{arrow}. If \code{wkb}, geometry is encoded in WKB
If \code{arrow}, then the geometry is encoded in GeoArrow format.}

\item{geo_covering}{\emph{\code{logical(1)}} \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}\cr
If \code{TRUE}, includes a \code{bbox} column in \code{x} and adds
the experimental covering metadata from the GeoParquet 1.1
specification.}
}
\value{
The input \code{x} invisibly
}
\description{
'\href{https://geoparquet.org/}{GeoParquet}' is a geospatial columnar
format backed by the '\href{https://parquet.apache.org/}{Parquet}' file format.
}
