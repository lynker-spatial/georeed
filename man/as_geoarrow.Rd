% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encoding-geoarrow.R
\name{as_geoarrow}
\alias{as_geoarrow}
\alias{as_geoarrow.sf}
\alias{as_geoarrow.sfc}
\title{Coerce object into a GeoArrow array}
\usage{
as_geoarrow(.x, ...)

\method{as_geoarrow}{sf}(.x, ...)

\method{as_geoarrow}{sfc}(.x, ...)
}
\arguments{
\item{.x}{\code{sf} or \code{sfc} object}

\item{...}{Unused}
}
\value{
An \link[arrow:Table-class]{arrow::Table} or \link[arrow:array-class]{arrow::Array}
}
\description{
Coerce object into a GeoArrow array
}
