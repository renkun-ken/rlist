#' Take a number of elements from a list or vector
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
  .data[0L:ifelse(force, n, min(length(.data), n))]
}

#' Take out elements until a condition is violated
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
  try(list.map.internal(.data, substitute(cond), parent.frame(), list.while.fun,
    args), silent = TRUE)
  .data[0L:args$i]
}
