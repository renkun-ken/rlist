#' Return a logical vector that indicates if each member of a list
#' satisfies a given condition
#'
#' @param x A list
#' @param cond A logical expression that specifies the condition
#' @param item The symbol to represent the list item, \code{.} in default
#' @param keep.names Whether to keep the names of list x
#' @name list.if
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.if(x,type=="B")
#' list.if(x,min(score$c1,score$c2) >= 8)
#' }
list.if <- function(x,cond,item=".",keep.names=TRUE) {
  cond <- substitute(cond)
  enclos <- new.env(FALSE,parent.frame(),1)
  result <- vapply(x,function(xi) {
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
    if(!is.logical(result)) stop("Undetermined condition")
    result
  },logical(1))
  if(!keep.names) names(result) <- NULL
  result
}
