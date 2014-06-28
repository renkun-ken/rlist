list.iter.internal <- function(x,expr) {
  l <- lambda(expr)
  genv <- new.env(FALSE,parent.frame(),3L)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  Map(function(...) {
    args <- `names<-`(list(...),l$symbols)
    enclos <- list2env(args,genv)
    env <- list.env(args[[1L]])
    eval(l$expr,env,enclos)
  },x,seq_along(x),xnames)
  invisible()
}
#' Iterate a list by evaluating an expression on
#' each list member.
#'
#' @param x The list to iterate
#' @param expr An expression that is evaluated for each item
#' @name list.iter
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.iter(x,cat(paste(type,"\n")))
#' list.iter(x,cat(str(.)))
#' }
list.iter <- function(x,expr) {
  expr <- substitute(expr)
  list.iter.internal(x,expr)
}
