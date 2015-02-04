. <- NULL

#' Return subsets of a list which meet conditions.
#'
#' @param x The list to be subsetted
#' @param subset A logical lambda expression of subsetting condition
#' @param select A lambda expression to evaluate for each selected item
#' @param ... Additional parameters
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' subset(x, type=='B')
#' subset(x, select=score)
#' subset(x, min(score$c1,score$c2) >= 8, data.frame(score))
#' do.call(rbind,
#'    subset(x,min(score$c1,score$c2) >= 8,data.frame(score)))
subset.list <- function(x, subset = TRUE, select = ., ...) {
  envir <- parent.frame()
  subset.items <- x[list.is.internal(x, substitute(subset), envir)]
  select.items <- list.map.internal(subset.items, substitute(select), envir)
  list.clean(select.items)
} 
