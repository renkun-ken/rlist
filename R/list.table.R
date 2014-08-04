#' Generate a table for a list by expression
#' @param .data \code{list}
#' @param ... A group of lambda expressions
#' @param table.args \code{list}. The additional parameters
#'    passed to \code{table}
#' @param .envir The environment to evaluate mapping function
#' @name list.table
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.table(x,type)
#' list.table(x,type,c1=score$c1)
#' list.table(x,type,score$c1,table.args=list(dnn=c("type","c1")))
#' }
list.table <- function(.data,...,table.args=NULL,.envir = parent.frame()) {
  args <- set_argnames(dots(...))
  items <- lapply(args,function(arg) {
    unlist(list.map.internal(.data,arg,envir = .envir),use.names = FALSE)
  })
  do.call(table,c(items,table.args))
}
