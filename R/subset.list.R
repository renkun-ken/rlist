. <- NULL

#' Return subsets of a list which meet conditions.
#'
#' @param x The list to be subsetted
#' @param subset A logical expression that specifies the subsetting condition
#' @param select An expression that is evaluated for each item
#' that satisfies the subsetting condition
#' @param ... Additional parameters
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
subset.list <- function(x,subset=TRUE,select=.,...) {
  lsubset <- lambda(substitute(subset))
  lselect <- lambda(substitute(select))
  envir.subset <- lambda.env(parent.frame())
  envir.select <- lambda.env(parent.frame())
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  items <- Map(function(...) {
    args <- list(...)
    names(args) <- lsubset$symbols
    list2env(args,envir.subset)
    env <- list.env(args[[1L]])
    result <- eval(lsubset$expr,env,envir.subset)
    if(is.logical(result)) {
      if(length(result) == 1L && result) {
        names(args) <- lselect$symbols
        list2env(args,envir.select)
        eval(lselect$expr,env,envir.select)
      } else if(length(result) > 1L) {
        stop("Multiple values are encountered")
      }
    }
  },x,seq_along(x),xnames)
  list.clean(items)
}
