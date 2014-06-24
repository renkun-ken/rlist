#' Reverse a list
#'
#' @param x The list to be reversed
#' @name list.reverse
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.reverse(x)
#' }
list.reverse <- function(x) {
  x[rev(seq_along(x))]
}
