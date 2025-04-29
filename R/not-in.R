#' Not In
#'
#' The opposite of [`%in%`].
#'
#' @param lhs A vector.
#' @param rhs A vector.
#'
#' @returns A logical vector with the same length as `lhs`.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3)
#' b <- c(1, 2)
#'
#' # logical vector
#' a %notin% b
#'
#' # equivalent to
#' !(a %in% b)
`%notin%` <- function(lhs, rhs) {
    !(lhs %in% rhs)
}
