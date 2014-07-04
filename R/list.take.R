#' Take a number of members from a list
#'
#' @param .data \code{list}
#' @param n The number of members to take out
#' @param force \code{TRUE} disables the length check
#' @name list.take
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.take(x,1)
#' list.take(x,10)
#' }
list.take <- function(.data,n,force=FALSE) {
  .data[0L:ifelse(force,n,min(length(.data),n))]
}
