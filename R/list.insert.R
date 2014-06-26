#' Insert a series of lists at the given index
#'
#' @param x A list
#' @param index The index at which the lists are inserted
#' @param ... A series of lists
#' @name list.insert
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.if(x,2,p2.1=list(type="B",score=list(c1=8,c2=9)))
#' }
list.insert <- function(x,index,...) {
  lists <- list(...)
  c(x[0:max(0,index-1)],lists,x[index:length(x)])
}
