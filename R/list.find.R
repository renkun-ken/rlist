#' Find a specific number of members in a list that meeting given condition
#'
#' @param .data \code{list}
#' @param cond A logical lambda expression
#' @param n The number of items to find
#' @param envir The environment to evaluate mapping function
#' @name list.find
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.find(x,type=="B",1)
#' list.find(x,min(score$c1,score$c2) >= 9)
#' }
list.find <- function(.data,cond,n=1L,envir=parent.frame()) {
  .data[list.findi.internal(.data,substitute(cond),n,envir)]
}
