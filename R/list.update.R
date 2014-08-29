#' Update a list by modifying its elements.
#'
#' @param .data \code{list}
#' @param ... A group of labmda expressions
#' @param keep.null Should \code{NULL} values be preserved
#'    for \code{modifyList}
#' @name list.update
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.update(x,high=max(score$c1,score$c2),low=min(score$c1,score$c2))
#' list.update(x,exams=length(score))
#' list.update(x,grade=ifelse(type=="A",score$c1,score$c2))
#' list.update(x,score=list(min=0,max=10))
#' }
list.update <- function(.data,...,keep.null=FALSE) {
  items <- lapply(dots(...),list.map.internal,
    .data=.data,envir = parent.frame())
  do.call(map,c(function(.data,...)
    modifyList(.data,list(...),keep.null = keep.null),list(.data),items))
}
