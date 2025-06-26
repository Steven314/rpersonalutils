#' Create a Temporary DuckDB Table
#'
#' Creates a caching system using temporary tables in DuckDB. This checks if a
#' table exists and creates it as a temp table if it does not, or can
#' recalculate if it does.
#'
#' This allows you to cache a calculation in DuckDB and retrieve it later
#' without inserting a check to calculate or to pull the existing. When
#' inserting the table must be materialized. You can't cache the result of
#' [`dplyr::tbl()`] until you [`dplyr::collect()`] it.
#'
#' This function may change or go away if/when `{duckplyr}` becomes more
#' friendly.
#'
#' @param con A DuckDB connection.
#' @param expr A tibble or dataframe-like object that can be inserted into
#'   DuckDB.
#' @param tbl_name The name for the temporary table.
#' @param overwrite A logical whether to overwrite an existing temporary table.
#' @param quiet Suppress information messages. The default is true and this does
#'   not stop any errors/warnings, only messages which inform the user on what
#'   is happening.
#'
#' @returns A lazy table from DuckDB and [`dplyr::tbl()`], invisibly.
#' @importFrom dplyr pull tbl
#' @export
temp_duck_table <- function(
    con,
    expr,
    tbl_name,
    overwrite = FALSE,
    quiet = TRUE
) {
    requireNamespace("duckdb", quietly = TRUE)

    if (!DBI::dbIsValid(con)) {
        cli::cli_abort("The connection is not valid.")
    }

    if (!is.character(tbl_name) & length(tbl_name) == 1) {
        cli::cli_abort("{.var tbl_name} must be a single string.")
    }

    # check if the table exists
    tbl_exists <- DBI::dbExistsTable(con, tbl_name)

    if (tbl_exists) {
        # check that the table is not named the same as an existing persistent
        # table.
        is_temp <- tbl(
            con,
            sql(
                paste(
                    "select table_name, temporary",
                    "from duckdb_tables()",
                    "where table_name =",
                    paste0("'", tbl_name, "'")
                )
            )
        ) |>
            pull("temporary")

        if (!is_temp) {
            cli::cli_abort(
                c(
                    "x" = paste(
                        "The table {.arg {tbl_name}} already exists as a",
                        "persistent table and cannot be used for a temporary",
                        "table."
                    ),
                    "i" = "Please choose a different name."
                )
            )
        }
    }

    if (!tbl_exists | (tbl_exists & overwrite)) {
        # if it does not exist, create the temp table.
        # if it does exists but is to be overwritten, do so.

        if (!quiet & !tbl_exists) {
            cli::cli_alert_info("The table does not exist, creating it.")
        }

        if (!quiet & tbl_exists & overwrite) {
            cli::cli_alert_info("The table already exists. Recalculating")
        }

        DBI::dbWriteTable(
            conn = con,
            name = tbl_name,
            value = expr,
            overwrite = overwrite,
            temporary = TRUE
        )
    }

    # otherwise do nothing; there is no need to recalculate.
    if (!quiet & tbl_exists & !overwrite) {
        cli::cli_alert_info("The table already exists.")
    }

    return(invisible(tbl(con, tbl_name)))
}
