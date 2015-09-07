#' Take a number of elements
#'
#' Take the first \code{n} elements out from a list or
#' vector.
#'
#' @param .data \code{list} or \code{vector}
#' @param n \code{integer}. The number of elements to take
#' @param force \code{TRUE} to disable the length check
#' @export
#' @seealso \code{\link{list.takeWhile}}, \code{\link{list.skip}},
#' \code{\link{list.skipWhile}}
#' @examples
#' x <- list(a=1,b=2,c=3)
#' list.take(x,1)
#' list.take(x,10)
list.take <- function(.data, n, force = FALSE) {
  if(!is.numeric(n)) stop("n must be numeric or integer", call. = FALSE)
  .data[0L:if(force) n else min(length(.data), n)]
}

#' Keep taking elements while a condition holds
#'
#' Keep taking elements out from a list or vector while
#' a condition holds for the element. If the condition is
#' violated for an element, the element will not be taken and
#' all taken elements will be returned.
#'
#' @param .data \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @export
#' @seealso \code{\link{list.take}}, \code{\link{list.skip}},
#' \code{\link{list.skipWhile}}
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.takeWhile(x, type=='B')
#' list.takeWhile(x, min(score$c1,score$c2) >= 8)
list.takeWhile <- function(.data, cond) {
  args <- args_env(i = 0L)
  tryWithCondition(list.map.internal(.data, substitute(cond), parent.frame(), list.while.fun,
    args), rlist.finished = NULL)
  .data[0L:args$i]
}
