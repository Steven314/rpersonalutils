# Geographic Distance Between Two Pairs of Coordinates

This wraps
[`geosphere::distGeo`](https://rdrr.io/cran/geosphere/man/distGeo.html)
and allows unit conversion. It is suitable for use in
[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html).
The vector lengths must be able to be recycled. Preferrably the
refenrece coordinates are each of length one or the same length as the
test coordinates.

## Usage

``` r
geographic_distance(lat, long, lat_ref, long_ref, unit = c("miles", "km"))
```

## Arguments

- lat:

  A vector of latitude coordinates.

- long:

  A vector of longitude coordinates.

- lat_ref:

  A reference latitude. May be a vector.

- long_ref:

  A reference longitude. May be a vector.

- unit:

  The output unit, default is `miles`. Must be miles or kilometers
  (`km`). The conversion factor is 1609.334 meters to 1 mile.

## Value

A vector of the distance between two points on a globe. Any missing
coordinates will return `NA`.

## Examples

``` r
coords <- c(36.68, -108.82) # somewhere in New Mexico
ref    <- c(45, -100)       # somewhere in South Dakota

if (FALSE) geographic_distance(coords[1], coords[2], ref[1], ref[2]) # \dontrun{}
```
