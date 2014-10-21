#' Map multiple lists with an expression
#'
#' @param expr An implicit lambda expression where only \code{.i} and
#'    \code{.name} are defined.
#' @param ... Named arguments of lists with equal length. The names of the
#'    lists are available as symbols that represent the element for each list.
#' @name list.maps
#' @export
#' @examples
#' \dontrun{
#' l1 <- list(p1=list(x=1,y=2), p2=list(x=3,y=4), p3=list(x=1,y=3))
#' l2 <- list(2,3,5)
#' list.maps(a$x*b+a$y,a=l1,b=l2)
#' list.maps(..1$x*..2+..1$y,l1,l2)
#' }
list.maps <- function(expr, ...) {
  expr <- substitute(expr)
  envir <- parent.frame()
  lists <- list(...)
  if(length(lists) == 0L) return(list())
  list1 <- lists[[1L]]
  xnames <- getnames(list1,character(1L))
  fun <- function(.expr, ...) eval(.expr, list(...))
  environment(fun) <- envir
  do.call("map", c(c(fun, lists), list(.expr = list(expr),
    .i = seq_along(list1), .name = xnames) ))
}
