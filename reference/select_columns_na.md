# Select Columns That Have Missing Values

Select Columns That Have Missing Values

## Usage

``` r
select_columns_na(.data, type = c("any", "all"))
```

## Arguments

- .data:

  A tibble.

- type:

  A string, either "any" or "all". Using "any" will take columns with
  one or more missing values. Using "all" will take only columns with
  only missing values.

## Value

A tibble.

## Examples

``` r
df <- tibble::tribble(
    ~ x, ~ y, ~ z,
    NA,   NA,   3,
    NA,    2,   3
)

select_columns_na(df, "any")
#> # A tibble: 2 × 2
#>   x         y
#>   <lgl> <dbl>
#> 1 NA       NA
#> 2 NA        2

select_columns_na(df, "all")
#> # A tibble: 2 × 1
#>   x    
#>   <lgl>
#> 1 NA   
#> 2 NA   
```
