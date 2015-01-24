#' Take a number of members from a list
#'
#' @param .data \code{list}
#' @param n \code{integer}. The number to take
#' @param force \code{TRUE} disables the length check
#' @export
#' @examples
#' x <- list(a=1,b=2,c=3)
#' list.take(x,1)
#' list.take(x,10)
list.take <- function(.data, n, force = FALSE) {
  .data[0L:ifelse(force, n, min(length(.data), n))]
}
