#' Iterate a list by evaluating an expression on
#' each list member.
#'
#' @param x The list to iterate
#' @param expr An expression that is evaluated for each item
#' @name list.iter
#' @return NULL
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
  l <- lambda(expr)
  enclos <- new.env(parent = parent.frame(),size = 1)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  items <- Map(function(xi,i,name) {
    enclos[[l$symbol]] <- xi
    enclos$.i <- i
    enclos$.name <- name
    env <- list.env(xi,enclos)
    eval(l$expr,env,enclos)
  },x,seq_along(x),xnames)
  invisible(NULL)
}
