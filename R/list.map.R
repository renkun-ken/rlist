list.map.internal <- function(x,expr) {
  l <- lambda(expr)
  genv <- new.env(FALSE,parent.frame(),3)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  Map(function(...) {
    args <- `names<-`(list(...),l$symbols)
    enclos <- list2env(args,genv)
    env <- list.env(args[[1]])
    eval(l$expr,env,enclos)
  },x,seq_along(x),xnames)
}

#' Map each member of a list by an expression.
#'
#' @param x The list to perform mapping
#' @param expr An expression that is evaluated for each item
#' @param keep.names Whether to keep the names of list x
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
list.map <- function(x,expr,keep.names=TRUE) {
  expr <- substitute(expr)
  items <- list.map.internal(x,expr)
  if(!keep.names) names(items) <- NULL
  items
}

#' Map each member of a list by an expression to a vector.
#'
#' @param ... The parameters passed to \code{list.map}
#' @param use.names Should the names of the results be preserved?
#' @name list.mapv
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.mapv(x,type)
#' list.mapv(x,min(score$c1,score$c2))
#' }
list.mapv <- function(...,use.names=TRUE) {
  unlist(list.map(...),use.names = use.names)
}
