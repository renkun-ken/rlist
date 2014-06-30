#' Iterate a list by evaluating an expression on
#' each list member.
#'
#' @param x The list to iterate
#' @param expr An expression that is evaluated for each item
#' @name list.iter
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.iter(x,cat(paste(type,"\n")))
#' list.iter(x,cat(str(.)))
#' }
list.iter <- function(x,expr) {
  expr <- substitute(expr)
  list.map.internal(x,expr)
  invisible()
}
