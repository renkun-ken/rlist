#' Flatten a list to only one level
#'
#' @param x \code{list}
#' @param use.names \code{logical}. Should the names of \code{x} be kept?
#' @name list.flatten
#' @export
#' @examples
#' \dontrun{
#' p <- list(a=1,b=list(b1=2,b2=3),c=list(c1=list(c11="a",c12="x"),c2=3))
#' list.flatten(p)
#' }
list.flatten <- function(x,use.names=TRUE) {
  len <- sum(rapply(x, function(x) 1L))
  y <- vector("list", len)
  i <- 0L
  items <- rapply(x, function(x) { i <<- i+1L; y[[i]] <<- x })
  if(use.names) names(y) <- names(items)
  y
}
