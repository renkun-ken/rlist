#' Take a number of members from a list
#'
#' @param x The list from which some members are taken
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
list.take <- function(x,n,force=FALSE) {
  x[0:ifelse(force,n,min(length(x),n))]
}
