#' Generate a table for a list by expression
#' @param .data \code{list}
#' @param ... The expressions to generate a table
#' @param table.args \code{list}. The additional parameters
#'    passed to \code{table}
#' @name list.table
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.table(x,type)
#' list.table(x,type,c1=score$c1)
#' }
list.table <- function(.data,...,table.args=NULL) {
  args <- set_argnames(dots(...))
  env <- parent.frame()
  items <- lapply(args,function(arg) {
    unlist(list.map.internal(.data,arg,envir = env),use.names = TRUE)
  })
  do.call(table,items)
}
