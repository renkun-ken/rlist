#' Give the indices of list elements satisfying
#' a given condition
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @return an \code{integer} vector
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.which(x, type == 'B')
#' list.which(x, min(score$c1,score$c2) >= 8)
list.which <- function(.data, cond) {
  which(list.is.internal(.data, substitute(cond), parent.frame()))
}
