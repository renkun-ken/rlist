#' Return a logical vector that indicates if each member of a list
#' satisfies a given condition
#'
#' @param x A list
#' @param cond A logical expression that specifies the condition
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
list.if <- function(x,cond,keep.names=TRUE) {
  cond <- substitute(cond)
  l <- lambda(cond)
  enclos <- new.env(FALSE,parent.frame(),1)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  results <- unlist(Map(function(xi,i,name) {
    assign(l$symbol,xi,envir = enclos)
    enclos$.i <- i
    enclos$.name <- name
    env <- list.env(xi,enclos)
    result <- eval(l$expr,env,enclos)
    if(length(result) > 1) stop("More than one results are returned")
    if(!is.logical(result)) stop("Undetermined condition")
    result
  },x,seq_along(x),xnames),use.names=keep.names)
  results
}
