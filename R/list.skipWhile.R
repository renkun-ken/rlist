#' Skip members until the given condition is broken
#'
#' @param .data \code{list}
#' @param cond The condition
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
list.skipWhile <- function(.data,cond) {
  cond <- substitute(cond)
  l <- lambda(cond)
  envir <- lambda.env(parent.frame())
  xnames <- names(.data)
  index <- 0L
  for(i in seq_along(.data)) {
    xi <- .data[[i]]
    args <- setnames(list(xi,i,xnames[i]),l$symbols)
    list2env(args,envir)
    env <- list.env(xi)
    result <- eval(l$expr,env,envir)
    if(is.logical(result)) {
      if(length(result) == 1L && result) index <- i
      else if(length(result) > 1L) stop("Multiple values are encountered")
      else if(length(result) == 0L) stop("Undetermined value")
      else break
    } else {
      stop("Results must be logical")
    }
  }
  .data[-(0L:index)]
}
