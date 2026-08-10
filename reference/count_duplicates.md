# Count Duplicate Rows

Count the number of rows and filter to show which combinations have
duplicates. This is just a wrapper around
[`dplyr::count`](https://dplyr.tidyverse.org/reference/count.html) and
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).

## Usage

``` r
count_duplicates(.data, ...)
```

## Arguments

- .data:

  A
  [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
  or data frame. There should not be a column with the name `n`. If
  there is, this won't work right.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to group by.

## Value

A tibble (or data frame) with columns from `...` and an `n` column with
the number of rows for each combination of `...`.

## Examples

``` r
mtcars |>
    count_duplicates(cyl)
#>   cyl  n
#> 1   8 14
#> 2   4 11
#> 3   6  7
```
