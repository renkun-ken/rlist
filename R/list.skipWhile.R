#' Skip members until the given condition is broken
#'
#' @param .data \code{list}
#' @param cond A logical lambda expression
#' @name list.skipWhile
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.skipWhile(x,type=="A")
#' list.skipWhile(x,min(score$c1,score$c2) >= 8)
#' }
list.skipWhile <- function(.data,cond) {
  .i <- 0L
  try(list.map.internal(.data,substitute(cond),
    list.while.fun,parent.frame()),silent = TRUE)
  .data[-(0L:.i)]
}
