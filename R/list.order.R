#' Return the order of each member in a list by expression
#'
#' @param .data \code{list}
#' @param ... A group of lambda expressions
#' @param keep.names Whether to keep the names of \code{x} in the result
#' @param na.last The way to deal with \code{NA}s.
#' @name list.order
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.order(x,type,desc(score$c2))
#' list.order(x,min(score$c1,score$c2))
#' list.order(x,min(score$c1,score$c2),keep.names=TRUE)
#' }
list.order <- function(.data, ..., keep.names = FALSE, na.last = TRUE) {
  result <- list.order.internal(.data, dots(...), parent.frame(),
    na.last = na.last)
  if(keep.names) setnames(result, names(.data)) else result
}
