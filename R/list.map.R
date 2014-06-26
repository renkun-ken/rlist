#' Map each member of a list by an expression.
#'
#' @param x The list to perform mapping
#' @param expr An expression that is evaluated for each item
#' @param item The symbol to represent the list item, \code{.} in default
#' @param keep.names Whether to keep the names of list x
#' @param keep.null Whether to keep \code{NULL} items in the result
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
list.map <- function(x,expr,
  item=".",keep.names=TRUE,keep.null=FALSE) {
  expr <- substitute(expr)
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
    eval(expr,env,enclos)
  })
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
