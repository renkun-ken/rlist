#' Skip members until the given condition is broken
#'
#' @param x The list
#' @param cond The condition
#' @param item The symbol to represent the list item, \code{.} in default
#' @param keep.names Whether to keep the names of list x
#' @param keep.null Whether to keep \code{NULL} items in the result
#' @name list.skipWhile
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.skipWhile(x,type=="A")
#' list.skipWhile(x,min(score$c1,score$c2) >= 8)
#' }
list.skipWhile <- function(x,cond=TRUE,
  item=".",keep.names=TRUE,keep.null=FALSE) {
  cond <- substitute(cond)
  enclos <- new.env(FALSE,parent.frame(),1)
  index <- 0
  for(i in seq_along(x)) {
    xi <- x[[i]]
    assign(item,xi,envir = enclos)
    if(is.list(xi) || is.environment(xi)) {
      env <- xi
    } else if(is.vector(xi)) {
      env <- as.list(xi)
    } else {
      env <- enclos
    }
    result <- eval(cond,env,enclos)
    if(length(result) > 1) stop("More than one results are returned")
    if(is.logical(result) && result) {
      index <- i
    } else {
      break
    }
  }
  items <- x[(index+1):length(x)]
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
