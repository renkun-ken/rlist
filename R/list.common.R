#' Get all common cases by expression for a list
#'
#' @param .data \code{list}
#' @param expr An anonymous (or "lambda") expression to determine common cases. If one
#' is not specified, \code{list.common} simply returns all identical sub-elements within lists.
#' @export
#' @examples
#' x <- list(c('a','b','c'),c('a','b'),c('b','c'))
#' list.common(x, .)
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.common(x,type)
#' list.common(x,names(score))
#'
#' foo <- list(x = LETTERS[1:3], y = LETTERS[3:5])
#' list.common(foo)
list.common <- function(.data, expr) {

  if (!length(.data)) {
    return(NULL)
  }

  if(missing(expr)){
    expr <- quote(.)
  } else {
    expr <- substitute(expr)
  }

  values <- list.map.internal(.data, expr, parent.frame())
  return(reduce(intersect, values, values[[1L]]))
}
