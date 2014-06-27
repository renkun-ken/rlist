#' Find the indices of a number of members in a list that
#' meet given condition
#'
#' @param x The list
#' @param cond The condition
#' @param n The maximal number of members to find out
#' @name list.findi
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.findi(x,type=="B")
#' list.findi(x,min(score$c1,score$c2) >= 8)
#' list.findi(x,min(score$c1,score$c2) <= 8,2)
#' }
list.findi <- function(x,cond,n=1) {
  cond <- substitute(cond)
  l <- lambda(cond)
  enclos <- new.env(FALSE,parent.frame(),1)
  indices <- integer()
  xnames <- names(x)
  for(i in seq_along(x)) {
    xi <- x[[i]]
    assign(l$symbol,xi,envir = enclos)
    enclos$.i <- i
    enclos$.name <- xnames[i]
    env <- list.env(xi,enclos)
    result <- eval(l$expr,env,enclos)
    if(length(result) > 1) stop("More than one results are returned")
    if(length(indices) < n) {
      if(!is.logical(result)) stop("Undetermined condition")
      if(result) {
        indices <- c(indices,i)
      }
    } else {
      break
    }
  }
  indices
}
