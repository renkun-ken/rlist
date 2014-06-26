#' Map each member of a list by an expression.
#'
#' @param x The list to perform mapping
#' @param expr An expression that is evaluated for each item
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
  keep.names=TRUE,keep.null=FALSE) {
  expr <- substitute(expr)
  l <- lambda(expr)
  enclos <- new.env(FALSE,parent.frame(),1)
  items <- lapply(x,function(xi) {
    assign(l$symbol,xi,envir = enclos)
    if(is.list(xi) || is.environment(xi)) {
      env <- xi
    } else if(is.vector(xi)) {
      env <- as.list(xi)
    } else {
      env <- enclos
    }
    eval(l$expr,env,enclos)
  })
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
