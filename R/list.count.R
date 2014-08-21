#' Count the number of members that meet given condition
#'
#' @param .data \code{list}
#' @param cond A logical lambda expression
#' @name list.count
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.count(x,type=="B")
#' list.count(x,min(unlist(score)) >= 9)
#' }
list.count <- function(.data,cond) {
  length(which(list.is.internal(.data,substitute(cond),envir = parent.frame())))
}
