#' Geographic Distance Between Two Pairs of Coordinates
#'
#' This wraps [`geosphere::distGeo`] and allows unit conversion.
#'
#' @param lat A vector of latitude coordinates.
#' @param long A vector of longitude coordinates.
#' @param lat_ref A reference latitude.
#' @param long_ref A reference longitude.
#' @param unit The output unit. Must be miles or kilometers (`km`). The
#'   conversion factor is 1609.334 meters to 1 mile.
#'
#' @returns A vector of the distance between to points on a globe.
#' @export
geographic_distance <- function(
    lat, long,
    lat_ref, long_ref,
    unit = c("miles", "km")
) {
    requireNamespace("geosphere", quietly = TRUE)

    unit <- rlang::arg_match(unit)

    # the unit for `distGeo`, divide by 1609.344 to get miles and 1000 to get
    # kilometers.
    geosphere::distGeo(
        cbind(long, lat),
        cbind(long_ref, lat_ref)
    ) / ifelse(unit == "miles", 1609.344, 1000)
}
