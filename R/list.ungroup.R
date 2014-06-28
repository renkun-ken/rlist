#' Ungroup a list
#'
#' @param x A list
#' @param sort.names Should the members be sorted after ungrouping?
#' @name list.ungroup
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' xg <- list.group(x,type)
#' list.ungroup(xg)
#' }
list.ungroup <- function(x,sort.names=FALSE) {
  names(x) <- NULL
  result <- unlist(x,recursive = FALSE)
  result.names <- names(result)
  if (sort.names && !is.null(result.names)) {
    result[sort(result.names)]
  } else {
    result
  }
}
