#' Take out members until the given condition is broken
#'
#' @param x The list
#' @param cond The condition
#' @param keep.names Whether to keep the names of list x
#' @param keep.null Whether to keep \code{NULL} items in the result
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
list.takeWhile <- function(x,cond=TRUE,
  keep.names=TRUE,keep.null=FALSE) {
  cond <- substitute(cond)
  l <- lambda(cond)
  enclos <- new.env(FALSE,parent.frame(),1)
  index <- 0
  for(i in seq_along(x)) {
    xi <- x[[i]]
    assign(l$symbol,xi,envir = enclos)
    env <- list.env(xi,enclos)
    result <- eval(l$expr,env,enclos)
    if(length(result) > 1) stop("More than one results are returned")
    if(!is.logical(result)) stop("Undetermined condition")
    if(result) {
      index <- i
    } else {
      break
    }
  }
  items <- x[0:index]
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
