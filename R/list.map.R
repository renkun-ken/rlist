#' Map each member of a list by an expression.
#'
#' @param .data \code{list}
#' @param expr A lambda expression
#' @name list.map
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.map(x,type)
#' list.map(x,min(score$c1,score$c2))
#' }
list.map <- function(.data,expr) {
  list.map.internal(.data,substitute(expr),envir = parent.frame())
}

#' Map each member of a list by an expression to a vector.
#'
#' @param .data \code{list}
#' @param expr The expression
#' @param use.names Should the names of the results be preserved?
#' @name list.mapv
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.mapv(x,type)
#' list.mapv(x,min(score$c1,score$c2))
#' }
list.mapv <- function(.data,expr,use.names=TRUE) {
  unlist(list.map.internal(.data,substitute(expr),envir = parent.frame()),
    use.names=use.names)
}
