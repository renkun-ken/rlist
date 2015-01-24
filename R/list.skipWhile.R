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
