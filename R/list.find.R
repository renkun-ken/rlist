#' Find a specific number of members in a list that meeting given condition
#'
#' @param x The list to be filtered
#' @param cond A logical expression that specifies the lookup condition
#' @param n The maximal number of members to return, 1 by default
#' @param item The symbol to represent the list item, \code{.} in default
#' @param keep.names Whether to keep the names of list x
#' @param keep.null Whether to keep \code{NULL} items in the result
#' @name list.find
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.find(x,type=="B",1)
#' list.find(x,min(score$c1,score$c2) >= 9)
#' }
list.find <- function(x,cond=TRUE,
  n=1,item=".",keep.names=TRUE,keep.null=FALSE) {
  cond <- substitute(cond)
  enclos <- new.env(FALSE,parent.frame(),1)
  items <- list()
  N <- 0
  for(i in seq_along(x)) {
    it <- x[i]
    assign(item,it,envir = enclos)
    if(is.list(it) || is.environment(it)) {
      env <- it
    } else if(is.vector(it)) {
      env <- as.list(it)
    } else {
      env <- enclos
    }
    result <- eval(cond,env,enclos)
    if(length(result) > 1) stop("More than one results are returned")
    if(N < n) {
      if(is.logical(result) && result) {
        items <- c(items,it)
        N <- N+1
      }
    } else {
      break
    }
  }
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
