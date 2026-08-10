# Create a Temporary DuckDB Table

Creates a caching system using temporary tables in DuckDB. This checks
if a table exists and creates it as a temp table if it does not, or can
recalculate if it does.

## Usage

``` r
temp_duck_table(.data, con, tbl_name, overwrite = FALSE, quiet = TRUE)
```

## Arguments

- .data:

  A tibble or dataframe-like object that can be inserted into DuckDB.

- con:

  A DuckDB connection.

- tbl_name:

  The name for the temporary table.

- overwrite:

  A logical whether to overwrite an existing temporary table.

- quiet:

  Suppress information messages. The default is true and this does not
  stop any errors/warnings, only messages which inform the user on what
  is happening.

## Value

A lazy table from DuckDB and
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html),
invisibly.

## Details

This allows you to cache a calculation in DuckDB and retrieve it later
without inserting a check to calculate or to pull the existing. When
inserting, the table does not have to be materialized. You can cache the
result of a DuckDB query which uses
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html).
Queries from any other database must be materialized.

This is similar to the concept of `duckplyr::compute.duckplyr_df`, but
is a little more flexible for my needs (see
[tidyverse/duckplyr#728](https://github.com/tidyverse/duckplyr/issues/728)).
