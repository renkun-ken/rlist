#' Generate a table for a list by expression
#' @param .data A \code{list} or \code{vector}
#' @param ... A group of lambda expressions
#' @param table.args \code{list}. The additional parameters
#'    passed to \code{table}
#' @export
#' @examples
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.table(x, type)
#' list.table(x, type, c1 = score$c1)
#' list.table(x, type, score$c1, table.args = list(dnn=c("type","c1")))
list.table <- function(.data, ..., table.args = list(useNA = "ifany")) {
  args <- set_argnames(dots(...))
  envir <- parent.frame()
  items <- lapply(args, function(arg) {
    values <- list.map.internal(.data, arg, envir)
    values[vapply(values, is.null, logical(1L))] <- NA
    unlist(values, use.names = FALSE)
  })
  do.call("table", c(items, table.args))
}
