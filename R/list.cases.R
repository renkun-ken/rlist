#' Get all unique cases by expression for a list
#'
#' @param .data A list
#' @param expr The expression to evaluate to find cases
#' @param ... Additional parameters passed to \code{unique}
#' @param simplify \code{logical}. Should the results be simplified? Only works
#'    when each member in the result is a vector of length 1.
#' @param sort \code{logical}. Should the cases be sorted in ascending order?
#'    Ignored when the result is atomic.
#' @name list.cases
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.cases(x,type)
#' list.cases(x,mean(unlist(score)))
#' }
list.cases <- function(.data,expr,...,simplify=TRUE,sort=TRUE) {
  values <- list.map.internal(.data,substitute(expr))
  cases <- unique(values,...)
  if(simplify) {
    lens <- vapply(cases,length,integer(1L),USE.NAMES = FALSE)
    if(all(lens <= 1L)) cases <- unlist(cases,use.names = FALSE)
  }
  if(sort && is.atomic(cases)) sort(cases) else cases
}
