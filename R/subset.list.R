#' Return subsets of a list which meet conditions.
#'
#' @param list The list to be subsetted
#' @param subset A logical expression that specifies the subsetting condition
#' @param select An expression that is evaluated for each item that satisfies the subsetting condition
#' @param ... Additional arguments
#' @name subset.list
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=10),
#'        p2 = list(type="B",score=8),
#'        p3 = list(type="B",score=7))
#' subset(x,type=="B")
#' subset(x,select=score)
#' subset(x,type=="B" & score>=8,paste(type,score,sep=":"))
#' }
subset.list <- function(list,subset=TRUE,select=NULL,...) {
  subset <- substitute(subset)
  select <- substitute(select)
  items <- lapply(list,function(item) {
    env <- list2env(item,parent = parent.frame(n=3))
    if(eval(subset,envir = env)) {
      if(is.null(select)) {
        item
      } else {
        eval(select,envir = env)
      }
    }
  })
  names(items) <- names(list)
  items[vapply(items,is.null,logical(1))] <- NULL
  items
}
