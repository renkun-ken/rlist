#' Map multiple lists with an expression
#'
#' @param expr \code{expression}. Does not accept lambda expression.
#' @param ... Named arguments of lists with equal length. The names of the
#'    lists are available as symbols that represent the element for each list.
#' @name list.maps
#' @export
#' @examples
#' \dontrun{
#' l1 <- list(p1=list(x=1,y=2), p2=list(x=3,y=4), p3=list(x=1,y=3))
#' l2 <- list(2,3,5)
#' list.maps(a$x*b+a$y,a=l1,b=l2)
#' }
list.maps <- function(expr,...) {
  expr <- substitute(expr)
  enclos <- parent.frame()
  do.call(Map, c(function(...) {
    eval(expr,list(...),enclos)
  },list(...)))
}
