#' Take out members until the given condition is broken
#'
#' @param x The list
#' @param cond The condition
#' @name list.takeWhile
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.takeWhile(x,type=="B")
#' list.takeWhile(x,min(score$c1,score$c2) >= 8)
#' }
list.takeWhile <- function(x,cond) {
  cond <- substitute(cond)
  l <- lambda(cond)
  envir <- new.env(FALSE,parent.frame(),3L)
  xnames <- names(x)
  index <- 0L
  for(i in seq_along(x)) {
    xi <- x[[i]]
    args <- `names<-`(list(xi,i,xnames[i]),l$symbols)
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
  x[0L:index]
}
