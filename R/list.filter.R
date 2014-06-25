#' Filter a list by a condition.
#'
#' @param x The list to be filtered
#' @param ... The parameters passed to \code{list.if}
#' @param keep.null Whether to keep \code{NULL} items in the result
#' @name list.filter
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.filter(x,type=="B")
#' list.filter(x,min(score$c1,score$c2) >= 8)
#' }
list.filter <- function(x,...,keep.null=FALSE) {
  items <- x[list.if(x,...)]
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
