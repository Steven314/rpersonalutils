# Collect Query from SQL File

From a SQL file, query the database and return the results in a tibble.

## Usage

``` r
collect_query(conn, path)
```

## Arguments

- conn:

  A database connection.

- path:

  File path to a SQL file.

## Value

A tibble containing the results of the query.

## Details

The SQL must not end with a semicolon. Whitespace and comments are
allowed.
