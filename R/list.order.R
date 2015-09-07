#' Give the order of each list element by expression
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... A group of lambda expressions
#' @param keep.names Whether to keep the names of \code{x} in the result
#' @param na.last The way to deal with \code{NA}s.
#' @export
#' @return an \code{integer} vector.
#' @seealso \code{\link{list.sort}}
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.order(x, type, (score$c2)) # order by type (ascending) and score$c2 (descending)
#' list.order(x, min(score$c1,score$c2))
#' list.order(x, min(score$c1,score$c2), keep.names=TRUE)
list.order <- function(.data, ..., keep.names = FALSE, na.last = TRUE) {
  result <- list.order.internal(.data, dots(...), parent.frame(), na.last = na.last)
  if (keep.names) setnames(result, names(.data)) else result
}

#' Sort a list by given expressions
#'
#' @param .data a \code{list} or \code{vector}
#' @param ... A group of lambda expressions. For each expression, the data
#' is sorted ascending by default unless the expression is enclosed by ().
#' @param na.last The way to deal with \code{NA}s.
#' @seealso \code{\link{list.order}}
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.sort(x, type, (score$c2)) # sort by score$c2 in descending order
#' list.sort(x, min(score$c1,score$c2))
list.sort <- function(.data, ..., na.last = NA) {
  .data[list.order.internal(.data, dots(...), parent.frame(), na.last = na.last)]
}
