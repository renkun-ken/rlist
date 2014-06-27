#' Group a list by the value of an expression evaluated for each member.
#'
#' @param x The list to group
#' @param key An expression that determines the key of the group
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
  keep.group.names=TRUE,keep.item.names=TRUE) {
  key <- substitute(key)
  l <- lambda(key)
  genv <- new.env(FALSE,parent.frame(),3)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  keys <- Map(function(...) {
    args <- `names<-`(list(...),l$symbols)
    enclos <- list2env(args,genv)
    env <- list.env(args[[1]])
    eval(l$expr,env,enclos)
  },x,seq_along(x),xnames)
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
