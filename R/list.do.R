#' Call a function with a list of arguments as provided
#'
#' @param .data \code{list}
#' @param fun The \code{function} to call
#' @param ... The additional parameters passed to \code{do.call}
#' @export
#' @examples
#' x <- lapply(1:3, function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3, function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.do(x, rbind)
list.do <- function(.data, fun, ...) {
  do.call(fun, as.list(.data), ...)
}

#' Bind all list elements by row
#'
#' @param .data \code{list}
#' @export
#' @seealso \code{\link{list.cbind}}, \code{\link{list.stack}}
#' @examples
#' x <- lapply(1:3,function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3,function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.rbind(x)
#' list.rbind(df)
list.rbind <- function(.data) {
  list.do(.data, "rbind")
}

#' Bind all list elements by column
#'
#' @param .data \code{list}
#' @export
#' @seealso \code{\link{list.cbind}}, \code{\link{list.stack}}
#' @examples
#' x <- list(data.frame(i=1:5,x=rnorm(5)),
#'    data.frame(y=rnorm(5),z=rnorm(5)))
#' list.cbind(x)
list.cbind <- function(.data) {
  list.do(.data, "cbind")
}

#' Apply a function to each list element (\code{lapply})
#' @export
#' @param .data A \code{list} or \code{vector}
#' @param .fun \code{function}
#' @param ... Additional parameters passed to \code{FUN}.
list.apply <- function(.data,.fun,...)
  lapply(X = .data, FUN = .fun, ...)
