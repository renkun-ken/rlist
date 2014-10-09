#' Get all unique cases by expression for a list
#'
#' @param .data \code{list}
#' @param ... keys
#' @param simplify \code{logical}. Should atomic vectors be simplified
#'    by \code{unlist}?
#' @param sort \code{logical}. Should the cases be sorted in ascending order?
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
list.cases <- function(.data, ..., simplify=TRUE, sort=TRUE) {
  args <- dots(...)
  envir <- parent.frame()
  values <- list.map.internal(.data,args[[1L]],envir = envir)
  if(simplify) {
    atomic <- vapply(values,is.atomic,logical(1L))
    if(all(atomic)) values <- unlist(values,use.names = FALSE)
  }
  cases <- unique(values)
  if(sort && is.atomic(cases)) sort(cases) else cases
}
