#' Geographic Distance Between Two Pairs of Coordinates
#'
#' This wraps [`geosphere::distGeo`](https://rdrr.io/cran/geosphere/man/distGeo.html) and allows unit conversion. It is suitable
#' for use in [`dplyr::mutate`]. The vector lengths must be able to be recycled.
#' Preferrably the refenrece coordinates are each of length one or the same
#' length as the test coordinates.
#'
#' @param lat A vector of latitude coordinates.
#' @param long A vector of longitude coordinates.
#' @param lat_ref A reference latitude. May be a vector.
#' @param long_ref A reference longitude. May be a vector.
#' @param unit The output unit, default is `miles`. Must be miles or kilometers
#'   (`km`). The conversion factor is 1609.334 meters to 1 mile.
#'
#' @returns A vector of the distance between two points on a globe. Any missing
#'   coordinates will return `NA`.
#' @importFrom rlang is_installed
#' @export
#'
#' @examples
#' coords <- c(36.68, -108.82) # somewhere in New Mexico
#' ref    <- c(45, -100)       # somewhere in South Dakota
#'
#' \dontrun{geographic_distance(coords[1], coords[2], ref[1], ref[2])}
geographic_distance <- function(
    lat,
    long,
    lat_ref,
    long_ref,
    unit = c("miles", "km")
) {
    if (!rlang::is_installed("geosphere")) {
        stop(
            "The package `geosphere` is required for this function, but is not installed.\n",
            "Please install the package with `install.packages('geosphere')`."
        )
    }

    unit <- rlang::arg_match(unit)

    # ensure that abs(latitudes) are less than 90 degrees.
    within_90 <- function(l) all(abs(l) <= 90 | is.na(l))
    stopifnot("Latitudes must be within [-90, 90]." = within_90(lat))
    stopifnot(
        "Reference latitudes must be within [-90, 90]." = within_90(lat_ref)
    )

    # the unit for `distGeo` is meters, divide by 1609.344 to get miles and 1000
    # to get kilometers.
    geosphere::distGeo(
        cbind(long, lat),
        cbind(long_ref, lat_ref)
    ) /
        ifelse(unit == "miles", 1609.344, 1000)
}
