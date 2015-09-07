#' Skip a number of elements
#'
#' Skip the first \code{n} elements of a list or vector and
#' return the remaining elements if any.
#'
#' @param .data A \code{list} or \code{vector}
#' @param n \code{integer}. The number of elements to skip
#' @export
#' @seealso \code{\link{list.skipWhile}}, \code{\link{list.take}},
#' \code{\link{list.takeWhile}}
#' @examples
#' x <- list(a=1,b=2,c=3)
#' list.skip(x, 1)
#' list.skip(x, 2)
list.skip <- function(.data, n) {
  if(!is.numeric(n)) stop("n must be numeric or integer", call. = FALSE)
  if(n > 0L) .data[-(1L:n)]
  else if(n < 0L) .data[1L:(-n)]
  else .data
}

#' Keep skipping elements while a condition holds
#'
#' Keep skipping elements in a list or vector while a
#' condition holds for the element. As long as the condition
#' is violated, the element will be kept and all remaining
#' elements are returned.
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @export
#' @seealso \code{\link{list.skip}}, \code{\link{list.take}},
#' \code{\link{list.takeWhile}}
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.skipWhile(x, type=='A')
#' list.skipWhile(x, min(score$c1,score$c2) >= 8)
list.skipWhile <- function(.data, cond) {
  args <- args_env(i = 0L)
  tryWithCondition(list.map.internal(.data, substitute(cond), parent.frame(), list.while.fun,
    args), rlist.finished = NULL)
  .data[-(0L:args$i)]
}
