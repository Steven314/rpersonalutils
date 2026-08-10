# Write a Table to a DuckDB Instance

Add or replace a (lazy) table in a DuckDB instance with the ability to
add a comment for a description of the table. This wraps
[`duckdb::dbWriteTable`](https://r.duckdb.org/reference/duckdb_connection-class.html)
and adds the comment via
[`dbExecute`](https://dbi.r-dbi.org/reference/dbExecute.html).

## Usage

``` r
write_duckdb(
  con,
  table,
  table_name,
  comment = NULL,
  overwrite = TRUE,
  ...,
  quiet = FALSE
)
```

## Arguments

- con:

  DuckDB connection. Default is the in-memory database.

- table:

  The table to add to the database. A
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) (or coercible
  to data.frame), or `tbl()`.

- table_name:

  A string to be the name of the table in the database.

- comment:

  A string or [`paste`](https://rdrr.io/r/base/paste.html)d vector of
  strings to be the comment on the table.

- overwrite:

  Whether to overwrite the table in the database if it already exists.

- ...:

  Other parameters passed on to
  [`duckdb::dbWriteTable`](https://r.duckdb.org/reference/duckdb_connection-class.html)
  or
  [`DBI::dbWriteTable`](https://dbi.r-dbi.org/reference/dbWriteTable.html).

- quiet:

  By default this will output confirmation of success. Set this argument
  to `TRUE` to silence those messages.

## Value

Invisibly returns `table`.
