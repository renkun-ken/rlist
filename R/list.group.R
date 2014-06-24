#' Group a list by the value of an expression evaluated for each member.
#'
#' @param x The list to be filtered
#' @param key An expression that determines the key of the group
#' @param item The symbol to represent the list item, \code{.} in default
#' @param keep.group.names Whether to keep the names of the groups
#' @param keep.item.names Whether to keep the names of the items in the result
#' @name list.group
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.group(x,type)
#' list.group(x,mean(unlist(score)))
#' }
list.group <- function(x,key,
  item=".",keep.group.names=TRUE,keep.item.names=TRUE) {
  key <- substitute(key)
  enclos <- new.env(FALSE,parent.frame(),1)
  keys <- lapply(x,function(i) {
    assign(item,i,envir = enclos)
    if(is.list(i) || is.environment(i)) {
      env <- i
    } else if(is.vector(i)) {
      env <- as.list(i)
    } else {
      env <- enclos
    }
    eval(key,env,enclos)
  })
  unikeys <- unique(keys)
  if(keep.group.names) names(unikeys) <- as.character(unikeys)
  groups <- lapply(unikeys,function(k) {
    selector <- vapply(keys,identical,logical(1),y=k)
    result <- x[selector]
    if(!keep.item.names) names(result) <- NULL
    result
  })
  groups
}
