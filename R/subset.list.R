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
  l.subset <- lambda(subset)
  l.select <- lambda(select)
  enclos.subset <- new.env(FALSE,parent.frame(),1)
  enclos.select <- new.env(FALSE,parent.frame(),1)
  items <- lapply(x,function(i) {
    assign(l.subset$symbol,i,envir = enclos.subset)
    if(is.list(i) || is.environment(i)) {
      env <- i
    } else if(is.vector(i)) {
      env <- as.list(i)
    } else {
      env <- enclos.subset
    }
    result <- eval(l.subset$expr,env,enclos.subset)
    if(length(result) > 1) stop("More than one results are returned")
    if(!is.logical(result)) stop("Undetermined condition")
    if(result) {
      assign(l.select$symbol,i,envir = enclos.select)
      eval(l.select$expr,env,enclos.select)
    }
  })
  if(!keep.names) names(items) <- NULL
  if(!keep.null) items[vapply(items,is.null,logical(1))] <- NULL
  items
}
