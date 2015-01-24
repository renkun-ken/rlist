#' Sort a list by given expressions in order
#'
#' @param .data \code{list}
#' @param ... A group of lambda expressions. For each expression, the data
#' is sorted ascending by default unless the expression is enclosed by ().
#' @param na.last The way to deal with \code{NA}s.
#' @seealso \code{\link{list.order}}
#' @export
#' @examples
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.sort(x, type, (score$c2)) # sort by score$c2 in descending order
#' list.sort(x, min(score$c1,score$c2))
list.sort <- function(.data, ..., na.last = NA) {
  .data[list.order.internal(.data, dots(...), parent.frame(),
    na.last = na.last)]
}
