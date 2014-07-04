#' Exclude members of a list that meet given condition.
#'
#' @param .data \code{list}
#' @param cond An \code{expression} that returns a logical value
#'    to exclude items
#' @name list.exclude
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.exclude(x,type=="B")
#' list.exclude(x,min(score$c1,score$c2) >= 8)
#' }
list.exclude <- function(.data,cond) {
  .data[!list.if.internal(.data,substitute(cond),FALSE)]
}
