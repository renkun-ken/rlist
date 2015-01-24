#' Skip a number of members in a list
#'
#' @param .data A \code{list} or \code{vector}
#' @param n \code{integer}. The number of elements to skip
#' @export
#' @examples
#' x <- list(a=1,b=2,c=3)
#' list.skip(x, 1)
#' list.skip(x, 2)
list.skip <- function(.data, n) {
  .data[-(1L:n)]
}
