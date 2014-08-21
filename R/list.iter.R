#' Iterate a list by evaluating an expression on
#' each list member.
#'
#' @param .data \code{list}
#' @param expr A lambda expression
#' @name list.iter
#' @export
#' @return \code{invisible(.data)}
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.iter(x,cat(paste(type,"\n")))
#' list.iter(x,cat(str(.)))
#' }
list.iter <- function(.data,expr) {
  list.map.internal(.data,substitute(expr),envir = parent.frame())
  invisible(.data)
}
