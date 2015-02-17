#' Subset a list by a logical condition
#'
#' @param x The list to subset
#' @param subset A logical lambda expression of subsetting condition
#' @param select A lambda expression to evaluate for each selected item
#' @param ... Additional parameters
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' subset(x, type == 'B')
#' subset(x, select = score)
#' subset(x, min(score$c1, score$c2) >= 8, data.frame(score))
#' subset(x, type == 'B', score$c1)
#' do.call(rbind,
#'    subset(x, min(score$c1, score$c2) >= 8, data.frame(score)))
subset.list <- function(x, subset, select, ...) {
  envir <- parent.frame()
  subset.items <- if(missing(subset)) x else
    x[list.is.internal(x, substitute(subset), envir)]
  select.items <- if(missing(select)) subset.items else
    list.map.internal(subset.items, substitute(select), envir)
  list.clean(select.items)
}
