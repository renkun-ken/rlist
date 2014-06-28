list.findi.internal <- function(x,cond,n) {
  l <- lambda(cond)
  genv <- new.env(FALSE,parent.frame(),3L)
  xnames <- names(x)
  indices <- integer()
  for(i in seq_along(x)) {
    xi <- x[[i]]
    args <- `names<-`(list(xi,i,xnames[i]),l$symbols)
    enclos <- list2env(args,genv)
    env <- list.env(xi)
    result <- eval(l$expr,env,enclos)
    if(length(indices) < n) {
      if(is.logical(result)) {
        if(length(result) == 1L && result) {
          indices <- c(indices,i)
        } else if(length(result) > 1L) {
          stop("Multiple values are encountered")
        }
      }
    } else {
      break
    }
  }
  indices
}

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
list.findi <- function(x,cond,n=1L) {
  cond <- substitute(cond)
  list.findi.internal(x,cond,n)
}
