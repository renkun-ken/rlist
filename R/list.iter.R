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
  enclos <- new.env(FALSE,parent.frame(),1)
  items <- lapply(x,function(i) {
    assign(l$symbol,i,envir = enclos)
    if(is.list(i) || is.environment(i)) {
      env <- i
    } else if(is.vector(i)) {
      env <- as.list(i)
    } else {
      env <- enclos
    }
    eval(l$expr,env,enclos)
  })
  invisible(NULL)
}
