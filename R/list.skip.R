#' Skip a number of members in a list
#'
#' @param x The list from which some members are taken
#' @param n The number of members to skip
#' @name list.skip
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.skip(x,1)
#' list.skip(x,2)
#' }
list.skip <- function(x,n) {
  x[-(1:n)]
}
