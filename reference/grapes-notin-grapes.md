# Not In

The opposite of [`%in%`](https://rdrr.io/r/base/match.html).

## Usage

``` r
lhs %notin% rhs
```

## Arguments

- lhs:

  A vector.

- rhs:

  A vector.

## Value

A logical vector with the same length as `lhs`.

## Examples

``` r
a <- c(1, 2, 3)
b <- c(1, 2)

# logical vector
a %notin% b
#> [1] FALSE FALSE  TRUE

# equivalent to
!(a %in% b)
#> [1] FALSE FALSE  TRUE
```
