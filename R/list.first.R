#' Find the first element that meets a condition
#' @param .data A \code{list} or \code{vector}
#' @param cond a logical lambda expression
#' @export
#' @seealso \code{\link{list.last}}
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.first(x, score$c1 < 10)
#' list.first(x, score$c1 < 9 || score$c3 >= 5) # NULL for all results are NA or FALSE
list.first <- function(.data, cond) {
  if (is.empty(.data))  return(NULL)
  if (missing(cond))  return(.data[[1L]])
  res <- list.first.internal(.data, substitute(cond), parent.frame(), na.rm = TRUE)
  res$value
}

#' Find the last element that meets a condition
#' @param .data A \code{list} or \code{vector}
#' @param cond a logical lambda expression
#' @seealso \code{\link{list.first}}
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.last(x, score$c1 < 10)
#' list.last(x, score$c1 < 9 || score$c3 >= 5) # NULL for all results are NA or FALSE
list.last <- function(.data, cond) {
  if (is.empty(.data)) return(NULL)
  if (missing(cond)) return(.data[[length(.data)]])
  res <- list.first.internal(rev(.data), substitute(cond), parent.frame(), na.rm = TRUE)
  res$value
}
