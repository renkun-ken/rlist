#' Select members of a list that match given regex pattern
#'
#' @param x The list to be match
#' @param pattern The regex pattern to match the name of the members
#' @param ... Additional parameters to pass to \code{grep}
#' @name list.match
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.match(x,"p[12]")
#' list.match(x,"3")
#' }
list.match <- function(x,pattern,...) {
  x[grep(pattern,names(x),...)]
}
