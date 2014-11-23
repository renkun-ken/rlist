#' Map each element in a list or vector by an expression.
#'
#' @param .data a \code{list} or \code{vector}
#' @param expr A lambda expression
#' @return A \code{list} in which each element is mapped by \code{expr} in \code{.data}
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.map(x,type)
#' list.map(x,min(score$c1,score$c2))
#' }
list.map <- function(.data, expr) {
  list.map.internal(.data, substitute(expr), parent.frame())
}

#' Map each member of a list by an expression to a vector.
#'
#' @param .data a \code{list} or \code{vector}
#' @param expr a lambda expression
#' @param as the mode to corece. Missing to \code{unlist}
#' the mapped results.
#' @param use.names Should the names of the results be preserved?
#' @return A \code{vector} in which each element is mapped by \code{expr} in \code{.data}
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.mapv(x,type)
#' list.mapv(x,min(score$c1,score$c2))
#' }
list.mapv <- function(.data, expr, as, use.names = TRUE) {
  res <- list.map.internal(.data, substitute(expr), parent.frame())
  if(missing(as)) unlist(res, use.names = use.names)
  else {
    res <- as.vector(res, as)
    if(use.names) names(res) <- names(.data)
    res
  }
}
