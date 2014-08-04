#' Find the indices of a number of members in a list that
#' meet given condition
#'
#' @param .data \code{list}
#' @param cond A logical lambda expression
#' @param n The maximal number of members to find out
#' @param envir The environment to evaluate mapping function
#' @name list.findi
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.findi(x,type=="B")
#' list.findi(x,min(score$c1,score$c2) >= 8)
#' list.findi(x,min(score$c1,score$c2) <= 8,2)
#' }
list.findi <- function(.data,cond,n=1L,envir = parent.frame()) {
  list.findi.internal(.data,substitute(cond),n,envir)
}
