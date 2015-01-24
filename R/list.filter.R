#' Filter a list by a condition.
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... logical conditions
#' @export
#' @examples
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.filter(x, type=="B")
#' list.filter(x, min(score$c1, score$c2) >= 8)
#' list.filter(x, type=="B", score$c2 >= 8)
list.filter <- function(.data,...) {
  conds <- dots(...)
  envir <- parent.frame()
  reduce(function(data, cond) {
    data[which(list.is.internal(data, cond, envir))]
  }, conds, .data)
}
