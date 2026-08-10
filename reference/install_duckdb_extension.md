# Install DuckDB Extensions Through HTTPS

The default core extension repository works through HTTP. There may be
network limitations on directly downloading a file in this way (such as
in an enterprise setting). To get around this, you can just add an 'S'
in the URL to make it use HTTPS. This function does that automatically
for you.

## Usage

``` r
install_duckdb_extension(
  duckdb_connection = duck_con(),
  extension_name,
  duckdb_version = as.character(packageVersion("duckdb")),
  platform_name = DBI::dbGetQuery(duckdb_connection, "PRAGMA platform")$platform
)
```

## Arguments

- duckdb_connection:

  The DuckDB connection. Defaults the the in-memory database.

- extension_name:

  Name of the extension to install. A list of extensions can be found in
  the [DuckDB
  documentation](https://duckdb.org/docs/stable/core_extensions/overview.html).

- duckdb_version:

  (Only needed for installing `httpfs`.) The version of DuckDB
  installed, don't include the 'v'.

- platform_name:

  (Only needed for installing `httpfs`.) See the
  [platforms](https://duckdb.org/docs/stable/dev/building/overview.html#platforms)
  for reference. This is checked against the list of platforms from the
  reference.

## Details

More
[info](https://duckdb.org/docs/stable/extensions/advanced_installation_methods)
about DuckDB extension installation.

The
[`httpfs`](https://duckdb.org/docs/stable/core_extensions/httpfs/overview.html)
extension needs to be installed first since it is used to install the
other extensions.
