# Manual DuckDB Table Creation

This is an internal function for creating new tables in DuckDB. This
avoids constructing a SQL statement each time.

## Usage

``` r
create_table_manually(con, name, value, overwrite = TRUE, temporary = FALSE)
```

## Arguments

- con:

  DuckDB connection.

- name:

  Table name.

- value:

  The table.

- overwrite:

  Overwrite.

- temporary:

  Temporary.

## Details

This is only for lazy tables, such as materializing a table from DuckDB
into DuckDB.
