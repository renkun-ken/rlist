. <- NULL

#' Return subsets of a list which meet conditions.
#'
#' @param x The list to be subsetted
#' @param subset A logical expression that specifies the subsetting condition
#' @param select An expression that is evaluated for each item
#' that satisfies the subsetting condition
#' @param keep.names Whether to keep the names of list x
#' @param keep.null Whether to keep \code{NULL} items in the result
#' @param ... Additional arguments
#' @name subset.list
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' subset(x,type=="B")
#' subset(x,select=score)
#' subset(x,min(score$c1,score$c2) >= 8,data.frame(score))
#' do.call(rbind,
#'    subset(x,min(score$c1,score$c2) >= 8,data.frame(score)))
#' }
subset.list <- function(x,subset=TRUE,select=.,
  keep.names=TRUE,keep.null=FALSE,...) {
  subset <- substitute(subset)
  select <- substitute(select)
  lsubset <- lambda(subset)
  lselect <- lambda(select)
  genv <- new.env(FALSE,parent.frame(),3)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  items <- Map(function(...) {
    args <- list(...)
    names(args) <- lsubset$symbols
    enclos <- list2env(args,genv)
    env <- list.env(args[[1]])
    result <- eval(lsubset$expr,env,enclos)
    if(is.logical(result)) {
      if(length(result) == 1L && result) {
        names(args) <- lselect$symbols
        enclos <- list2env(args,genv)
        eval(lselect$expr,env,enclos)
      } else if(length(result) > 1L) {
        stop("Multiple values are encountered")
      }
    }
  },x,seq_along(x),xnames)
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
