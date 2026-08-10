# Attach an Additional DuckDB File to Connection

Attach an Additional DuckDB File to Connection

## Usage

``` r
attach_duckdb(
  con,
  path,
  name = sub("\\..*$", "", basename(path)),
  read_only = FALSE
)
```

## Arguments

- con:

  DuckDB connection.

- path:

  Path of the new DuckDB file.

- name:

  Name to use for the new database. Defaults the name of the file.

- read_only:

  Read only. Defaults to false.

## Value

A single value vector with a value of zero if successful.
