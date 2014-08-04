#' Filter a list by a condition.
#'
#' @param .data \code{list}
#' @param cond A logical lambda expression
#' @param envir The environment to evaluate mapping function
#' @name list.filter
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.filter(x,type=="B")
#' list.filter(x,min(score$c1,score$c2) >= 8)
#' }
list.filter <- function(.data,cond,envir = parent.frame()) {
  list.clean(.data[list.is.internal(.data,substitute(cond),envir)])
}
