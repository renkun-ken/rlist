#' Merge two lists
#' @param x The original list
#' @param val A vector or list that is to be appended after \code{x}
#' @param keep.null Whether to keep \code{NULL} items
#' @name list.merge
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=list(x=1,y=2))
#' list.merge(x,list(b=5))
#' list.merge(x,list(c=list(z=3)))
#' }
list.merge <- modifyList

## implement mutiple merge
