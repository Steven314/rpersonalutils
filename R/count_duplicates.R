#' Count Duplicate Rows
#'
#' Count the number of rows and filter to show which combinations have
#' duplicates. This is just a wrapper around [`dplyr::count`] and
#' [`dplyr::filter`].
#'
#' @param .data A [`tibble::tibble`] or data frame. There should not be a column
#'   with the name `n`. If there is, this won't work right.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables to group
#'   by.
#'
#' @returns A tibble (or data frame) with columns from `...` and an `n` column
#'   with the number of rows for each combination of `...`.
#' @importFrom dplyr count filter
#' @export
#'
#' @examples
#' mtcars |>
#'     count_duplicates(cyl)
count_duplicates <- function(.data, ...) {
    .data |>
        dplyr::count(..., sort = TRUE) |>
        dplyr::filter(.data[["n"]] > 1)
}
