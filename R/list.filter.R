#' Filter a list by a condition.
#'
#' @param x The list to be filtered
#' @param subset A logical expression that specifies the filtering condition
#' @param item The symbol to represent the list item, \code{.} in default
#' @param keep.names Whether to keep the names of list x
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
list.filter <- function(x,filter=TRUE,
  item=".",keep.names=TRUE,keep.null=FALSE) {
  filter <- substitute(filter)
  enclos <- new.env(FALSE,parent.frame(),1)
  items <- lapply(x,function(i) {
    assign(item,i,envir = enclos)
    if(is.list(i) || is.environment(i)) {
      env <- i
    } else if(is.vector(i)) {
      env <- as.list(i)
    } else {
      env <- enclos
    }
    result <- eval(filter,env,enclos)
    if(length(result) > 1) stop("More than one results are returned")
    if(is.logical(result) && result) {
      i
    }
  })
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
