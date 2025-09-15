#' Install DuckDB Extensions Through HTTPS
#'
#' The default core extension repository works through HTTP. There may be
#' network limitations on directly downloading a file in this way (such as in an
#' enterprise setting). To get around this, you can just add an 'S' in the URL
#' to make it use HTTPS. This function does that automatically for you.
#'
#' More
#' [info](https://duckdb.org/docs/stable/extensions/advanced_installation_methods)
#' about DuckDB extension installation.
#'
#' The
#' [`httpfs`](https://duckdb.org/docs/stable/core_extensions/httpfs/overview.html)
#' extension needs to be installed first since it is used to install the other
#' extensions.
#'
#' @param duckdb_connection The DuckDB connection. Defaults the the in-memory
#'   database.
#' @param extension_name Name of the extension to install. A list of extensions
#'   can be found in the [DuckDB
#'   documentation](https://duckdb.org/docs/stable/core_extensions/overview.html).
#' @param duckdb_version (Only needed for installing `httpfs`.) The version of
#'   DuckDB installed, don't include the 'v'.
#' @param platform_name (Only needed for installing `httpfs`.) See the
#'   [platforms](https://duckdb.org/docs/stable/dev/building/overview.html#platforms)
#'   for reference. This is checked against the list of platforms from the
#'   reference.
#'
#' @importFrom utils download.file packageVersion
#' @export
install_duckdb_extension <- function(
    duckdb_connection = duck_con(),
    extension_name,
    duckdb_version = as.character(packageVersion("duckdb")),
    platform_name = DBI::dbGetQuery(
        duckdb_connection,
        "PRAGMA platform"
    )$platform
) {
    platform_name <- rlang::arg_match(
        platform_name,
        values = c(
            # full support
            "windows_amd64",
            "windows_arm64",
            "linux_amd64",
            "linux_arm64",
            "osx_amd64",
            "osx_arm64",
            # partial support
            "linux_amd64_musl",
            "linux_arm64_musl",
            "linux_arm64_android",
            "wasm_eh",
            # best effort support
            "freebsd_amd64",
            "freebsd_arm64",
            "wasm_mvp",
            "windows_amd64_mingw",
            "windows_arm64_mingw"
        )
    )

    # Download httpfs directly and install from file. You can't install it from
    # the HTTPS endpoint without having the extension already... ironic.
    httpfs_installed <- check_duckdb_extension(
        duckdb_connection,
        "httpfs",
        "installed"
    )

    if (!httpfs_installed) {
        if (!file.exists("httpfs.duckdb_extension.gz")) {
            cli::cli_alert_info("Downloading the `httpfs` extension.")

            download.file(
                paste0(
                    "https://extensions.duckdb.org/v",
                    duckdb_version,
                    "/",
                    platform_name,
                    "/",
                    "httpfs.duckdb_extension.gz"
                ),
                "httpfs.duckdb_extension.gz"
            )
        }

        cli::cli_alert_info("Installing `httpfs` from the downloaded file.")

        DBI::dbExecute(
            duckdb_connection,
            "INSTALL 'httpfs.duckdb_extension.gz'"
        )
    }

    # Check if the desired extension is already installed.
    check_install_sql <- sql(paste0(
        "from duckdb_extensions() where installed and extension_name = ",
        "'",
        extension_name,
        "'"
    ))

    already_installed <- check_duckdb_extension(
        duckdb_connection,
        extension_name,
        "installed"
    )

    if (!already_installed) {
        DBI::dbExecute(
            duckdb_connection,
            paste0(
                "INSTALL ",
                extension_name,
                " FROM 'https://extensions.duckdb.org'"
            )
        )

        post_install_check <- check_duckdb_extension(
            duckdb_connection,
            extension_name,
            "installed"
        )

        if (!post_install_check) {
            cli::cli_alert_danger(
                "The extension was not found. The installation seems to have failed."
            )
        }
    }
}

check_duckdb_extension <- function(
    duckdb_connection,
    extension_name,
    type = c("installed", "loaded")
) {
    type <- rlang::arg_match(type)

    check_install_sql <- sql(paste0(
        "from duckdb_extensions() where ",
        type,
        " and extension_name = ",
        "'",
        extension_name,
        "'"
    ))

    res <- DBI::dbGetQuery(duckdb_connection, check_install_sql) |> nrow()

    return(as.logical(res))
}
