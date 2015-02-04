#' Remove members from a list by index or name
#'
#' @param .data A \code{list} or \code{vector}
#' @param range A numeric vector of indices or
#' a character vector of names to remove from \code{.data}
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.remove(x, 'p1')
#' list.remove(x, c(1,2))
list.remove <- function(.data, range = integer()) {
  if (is.logical(range)) {
    .data[!range]
  } else if (is.numeric(range)) {
    .data[-range]
  } else if (is.character(range)) {
    names <- names(.data)
    m <- vapply(range, "==", logical(length(.data)), names)
    selector <- apply(m, 1L, any)
    .data[!selector]
  }
}

#' Exclude members of a list that meet given condition.
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression to exclude items
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.exclude(x, type=='B')
#' list.exclude(x, min(score$c1,score$c2) >= 8)
list.exclude <- function(.data, cond) {
  .data[!list.is.internal(.data, substitute(cond), parent.frame())]
} 
