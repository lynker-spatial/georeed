% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/georeed-parquet-metadata.R
\name{geoparquet_column_metadata}
\alias{geoparquet_column_metadata}
\title{Create GeoParquet column metadata}
\usage{
geoparquet_column_metadata(encoding, geometry_types, ...)
}
\arguments{
\item{encoding}{\emph{\code{character(1)}}\cr
Name of the geometry encoding format.
Currently only "WKB" is supported.}

\item{geometry_types}{\emph{\code{character(N)}}\cr
The geometry types of all geometries,
or an empty array if they are not known.}

\item{...}{Additional named arguments included in the returned list.}
}
\value{
\code{list}
}
\description{
Create GeoParquet column metadata
}
\seealso{
geoparquet_file_metadata
}
\keyword{internal}
