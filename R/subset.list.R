#' Return subsets of a list which meet conditions.
#'
#' @param x The list to be subsetted
#' @param subset A logical expression that specifies the subsetting condition
#' @param select An expression that is evaluated for each item that satisfies the subsetting condition
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
subset.list <- function(x,subset=TRUE,select=NULL,...) {
  subset <- substitute(subset)
  select <- substitute(select)
  enclos <- new.env(FALSE,parent.frame(),1)
  items <- lapply(x,function(item) {
    enclos$. <- item
    if(is.list(item) || is.environment(item)) {
      env <- item
    } else if(is.vector(item)) {
      env <- as.list(item)
    } else {
      env <- enclos
    }
    result <- eval(subset,env,enclos)
    if(length(result) > 1) stop("More than one results are returned")
    if(result) {
      if(is.null(select)) {
        item
      } else {
        eval(select,env,enclos)
      }
    } else {
      NULL
    }
  })
  names(items) <- names(x)
  items[vapply(items,is.null,logical(1))] <- NULL
  items
}
