#' Skip a number of members in a list
#'
#' @param .data A \code{list} or \code{vector}
#' @param n \code{integer}. The number of elements to skip
#' @export
#' @examples
#' x <- list(a=1,b=2,c=3)
#' list.skip(x, 1)
#' list.skip(x, 2)
list.skip <- function(.data, n) {
  .data[-(1L:n)]
}

#' Skip members until the given condition is broken
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression
#' @export
#' @examples
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.skipWhile(x, type=="A")
#' list.skipWhile(x, min(score$c1,score$c2) >= 8)
list.skipWhile <- function(.data, cond) {
  args <- args_env(i = 0L)
  try(list.map.internal(.data, substitute(cond), parent.frame(),
    list.while.fun, args), silent = TRUE)
  .data[-(0L:args$i)]
}
