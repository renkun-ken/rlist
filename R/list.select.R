#' Select by name or expression for each member of a list
#'
#' @param .data \code{list}
#' @param ... A group of implicit labmda expressions
#' @name list.select
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.select(x,type)
#' list.select(x,tp=type)
#' list.select(x,type,score)
#' list.select(x,type,score.range=range(unlist(score)))
#' }
list.select <- function(.data,...) {
  args <- set_argnames(dots(...))
  quote <- as.call(c(quote(list),args))
  list.map.internal(.data,quote)
}
