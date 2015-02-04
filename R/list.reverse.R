#' Reverse a list
#'
#' @param .data A \code{list} or \code{vector}
#' @export
#' @examples
#' x <- list(a=1,b=2,c=3)
#' list.reverse(x)
list.reverse <- function(.data) {
  .data[rev.default(seq_along(.data))]
} 
