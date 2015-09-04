#' Examine if a condition is true for all elements of a list
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @param na.rm logical. If true \code{NA} values are ignored in
#' the evaluation.
#' @seealso \code{\link{list.any}}
#' @return \code{TRUE} if \code{cond} is evaluated to be \code{TRUE}
#' for all elements in \code{.data}.
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.all(x, type=='B')
#' list.all(x, mean(unlist(score))>=6)
#' list.all(x, score$c2 > 8 || score$c3 > 5, na.rm = TRUE)
#' list.all(x, score$c2 > 8 || score$c3 > 5, na.rm = FALSE)
list.all <- function(.data, cond, na.rm = FALSE) {
  if (missing(.data))
    return(all(na.rm = na.rm))
  if (is.empty(.data) || missing(cond))
    return(all(.data, na.rm = na.rm))
  l <- lambda(substitute(cond))
  l$expr <- as.call(list(quote(`!`), l$expr))
  res <- list.first.internal(.data, l, parent.frame(), na.rm = na.rm)
  !res$state
}

#' Examine if a condition is true for at least one list element
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @param na.rm logical. If true \code{NA} values are ignored in
#' the evaluation.
#' @seealso \code{\link{list.all}}
#' @return \code{TRUE} if \code{cond} is evaluated to be \code{TRUE}
#' for any element in \code{.data}.
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.any(x,type=='B')
#' list.any(x,mean(unlist(score))>=6)
#' list.any(x, score$c2 > 8 || score$c3 > 5, na.rm = TRUE)
#' list.any(x, score$c2 > 8 || score$c3 > 5, na.rm = FALSE)
list.any <- function(.data, cond, na.rm = FALSE) {
  if (missing(.data))
    return(any(na.rm = na.rm))
  if (is.empty(.data) || missing(cond))
    return(any(.data, na.rm = na.rm))
  res <- list.first.internal(.data, substitute(cond), parent.frame(), na.rm = na.rm)
  res$state
}
