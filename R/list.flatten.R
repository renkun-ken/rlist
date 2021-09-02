#' Flatten a nested list to a one-level list
#'
#' @details
#' The function is essentially a slightly modified version of \code{flatten2}
#' provided by Tommy at \href{https://stackoverflow.com/a/8139959/2906900}{stackoverflow.com} who
#' has full credit of the implementation of this function.
#' @param x \code{list}
#' @param use.names \code{logical}. Should the names of \code{x} be kept?
#' @param classes A character vector of class names, or "ANY" to match any class.
#' @author \href{https://stackoverflow.com/users/662787/tommy}{Tommy}
#' @export
#' @examples
#' p <- list(a=1,b=list(b1=2,b2=3),c=list(c1=list(c11='a',c12='x'),c2=3))
#' list.flatten(p)
#'
#' p <- list(a=1,b=list(x="a",y="b",z=10))
#' list.flatten(p, classes = "numeric")
#' list.flatten(p, classes = "character")
list.flatten <- function(x, use.names = TRUE, classes = "ANY") {
  len <- sum(rapply(x, function(x) 1L, classes = classes))
  y <- vector("list", len)
  i <- 0L
  items <- rapply(x, function(x) {
    i <<- i + 1L
    y[[i]] <<- x
    TRUE
  }, classes = classes)
  if (use.names && !is.null(nm <- names(items)))
    names(y) <- nm
  y
}
