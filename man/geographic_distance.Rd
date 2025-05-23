% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geographic_distance.R
\name{geographic_distance}
\alias{geographic_distance}
\title{Geographic Distance Between Two Pairs of Coordinates}
\usage{
geographic_distance(lat, long, lat_ref, long_ref, unit = c("miles", "km"))
}
\arguments{
\item{lat}{A vector of latitude coordinates.}

\item{long}{A vector of longitude coordinates.}

\item{lat_ref}{A reference latitude. May be a vector.}

\item{long_ref}{A reference longitude. May be a vector.}

\item{unit}{The output unit, default is \code{miles}. Must be miles or kilometers
(\code{km}). The conversion factor is 1609.334 meters to 1 mile.}
}
\value{
A vector of the distance between two points on a globe. Any missing
coordinates will return \code{NA}.
}
\description{
This wraps \href{https://rdrr.io/cran/geosphere/man/distGeo.html}{\code{geosphere::distGeo}} and allows unit conversion. It is suitable
for use in \code{\link[dplyr:mutate]{dplyr::mutate}}. The vector lengths must be able to be recycled.
Preferrably the refenrece coordinates are each of length one or the same
length as the test coordinates.
}
\examples{
coords <- c(36.68, -108.82) # somewhere in New Mexico
ref    <- c(45, -100)       # somewhere in South Dakota

\dontrun{geographic_distance(coords[1], coords[2], ref[1], ref[2])}
}
