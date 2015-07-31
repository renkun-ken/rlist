#' Flatten a nested list to a one-level list
#'
#' @details
#' The function is essentially a slightly modified version of \code{flatten2}
#' provided by Tommy at \link{http://stackoverflow.com/a/8139959/2906900} who
#' has full credit of the implementation of this function.
#' @param x \code{list}
#' @param use.names \code{logical}. Should the names of \code{x} be kept?
#' @author Tommy (\link{http://stackoverflow.com/users/662787/tommy})
#' @export
#' @examples
#' p <- list(a=1,b=list(b1=2,b2=3),c=list(c1=list(c11='a',c12='x'),c2=3))
#' list.flatten(p)
list.flatten <- function(x, use.names = TRUE) {
  len <- sum(rapply(x, function(x) 1L))
  y <- vector("list", len)
  i <- 0L
  items <- rapply(x, function(x) {
    i <<- i + 1L
    y[[i]] <<- x
    TRUE
  })
  if (use.names)
    names(y) <- names(items)
  y
}
