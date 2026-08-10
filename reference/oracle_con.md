# Oracle Database Connection

Connect to an Oracle database. Provide the username and password through
secret means.

## Usage

``` r
oracle_con(user, pass, dbname)
```

## Arguments

- user:

  Oracle username.

- pass:

  Oracle password.

- dbname:

  Database name.

## Value

An ROracle database connection (`OraConnection`).

## Details

Installing the `ROracle` package can be complicated. It requires the OCI
library. Instructions can be found on the Oracle website and require an
Oracle account. The [version on
CRAN](https://cran.r-project.org/package=ROracle) is outdated, see
[Oracle](https://www.oracle.com/database/technologies/roracle-downloads.html)
for the most recent release.
