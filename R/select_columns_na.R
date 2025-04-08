#' Select Columns That Have Missing Values
#'
#' @param .data A tibble.
#' @param type A string, either "any" or "all". Using "any" will take columns
#'   with one or more missing values. Using "all" will take only columns with
#'   only missing values.
#'
#' @returns A tibble.
#' @importFrom dplyr select
#' @importFrom tidyselect where
#' @importFrom rlang arg_match
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'     ~ x, ~ y, ~ z,
#'     NA,   NA,   3,
#'     NA,    2,   3
#' )
#'
#' select_columns_na(df, "any")
#'
#' select_columns_na(df, "all")
select_columns_na <- function(.data, type = c("any", "all")) {
    type <- rlang::arg_match(type)

    if (type == "any") {
        return(
            .data |>
                dplyr::select(tidyselect::where(~ any(is.na(.x))))
        )
    }
    if (type == "all") {
        return(
            .data |>
                dplyr::select(tidyselect::where(~ all(is.na(.x))))
        )
    }
}
