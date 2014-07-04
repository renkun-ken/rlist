#' Reverse a list
#'
#' @param .data \code{list}
#' @name list.reverse
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.reverse(x)
#' }
list.reverse <- function(.data) {
  .data[rev(seq_along(.data))]
}
