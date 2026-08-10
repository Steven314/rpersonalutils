# DuckDB Connection

Connect to a DuckDB file.

## Usage

``` r
duck_con(dbdir, read_only = FALSE)
```

## Arguments

- dbdir:

  Location for database files. Should be a path to an existing directory
  in the file system. With the default (or ""), all data is kept in RAM.

- read_only:

  Set to TRUE for read-only operation. For file-based databases, this is
  only applied when the database file is opened for the first time.
  Subsequent connections (via the same drv object or a drv object
  pointing to the same path) will silently ignore this flag.

## Value

A DuckDB connection object (`duckdb::duckdb_driver`).
