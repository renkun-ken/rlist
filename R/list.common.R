#' Get all common cases by expression for a list
#'
#' @param .data \code{list}
#' @param expr A lambda expression
#' @export
#' @examples
#' x <- list(c('a','b','c'),c('a','b'),c('b','c'))
#' list.common(x, .)
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.common(x,type)
#' list.common(x,names(score))
list.common <- function(.data, expr) {
  if (length(.data) == 0L) 
    return(NULL)
  values <- list.map.internal(.data, substitute(expr), parent.frame())
  reduce(intersect, values, values[[1L]])
} 
