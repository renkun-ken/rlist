#' Get all unique cases by expression for a list
#'
#' @param x A list
#' @param expr The expression to evaluate to find cases
#' @param sort Should the cases be sorted in ascending order?
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
list.cases <- function(x,expr,sort=TRUE) {
  expr <- substitute(expr)
  cases <- unique(unlist(list.map.internal(x,expr),use.names = FALSE))
  if(sort) {
    sort(cases)
  } else {
    cases
  }
}
