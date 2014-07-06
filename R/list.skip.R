#' Skip a number of members in a list
#'
#' @param .data \code{list}
#' @param n \code{integer}. The number to skip
#' @name list.skip
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.skip(x,1)
#' list.skip(x,2)
#' }
list.skip <- function(.data,n) {
  .data[-(1L:n)]
}
