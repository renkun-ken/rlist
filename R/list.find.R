#' Find a specific number of members in a list that meeting given condition
#'
#' @param x The list to be filtered
#' @param cond A logical expression that specifies the lookup condition
#' @param n The maximal number of members to return, 1 by default
#' @param item The symbol to represent the list item, \code{.} in default
#' @param keep.names Whether to keep the names of list x
#' @param keep.null Whether to keep \code{NULL} items in the result
#' @name list.find
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.find(x,type=="B",1)
#' list.find(x,min(score$c1,score$c2) >= 9)
#' }
list.find <- function(x,cond=TRUE,
  n=1,item=".",keep.names=TRUE,keep.null=FALSE) {
  cond <- substitute(cond)
  enclos <- new.env(FALSE,parent.frame(),1)
  items <- list()
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
    if(length(items) < n) {
      if(is.logical(result) && result) {
        items <- c(items,x[i])
      }
    } else {
      break
    }
  }
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
