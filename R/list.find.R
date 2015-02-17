#' Find a specific number of elements in a list or vector
#' satisfying a given condition
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @param n The number of items to find. (\code{n = 1L} by default)
#' @return A list or vector of at most \code{n} elements in \code{.data}
#' found to satisfy \code{cond}.
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.find(x, type=='B', 1)
#' list.find(x, min(score$c1,score$c2) >= 9)
list.find <- function(.data, cond, n = 1L) {
  .data[list.findi.internal(.data, substitute(cond), parent.frame(), n)]
}

#' Find the indices of a number of elements in a list or vector
#' satisfying a given condition
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @param n The number of items to find. (\code{n = 1L} by default)
#' @return an integer vector consisting of the elements indices
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.findi(x, type=='B')
#' list.findi(x, min(score$c1,score$c2) >= 8)
#' list.findi(x, min(score$c1,score$c2) <= 8, n = 2)
list.findi <- function(.data, cond, n = 1L) {
  list.findi.internal(.data, substitute(cond), parent.frame(), n)
}
