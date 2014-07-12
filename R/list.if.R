#' Return a logical vector that indicates if each member of a list
#' satisfies a given condition
#'
#' @param .data \code{list}
#' @param cond A logical lambda expression
#' @param use.names \code{logical} Should the names of \code{.data} be kept?
#' @name list.if
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.if(x,type=="B")
#' list.if(x,min(score$c1,score$c2) >= 8)
#' }
list.if <- function(.data,cond,use.names=TRUE) {
  items <- list.if.internal(.data,substitute(cond))
  if(use.names) setnames(items,names(.data)) else items
}
