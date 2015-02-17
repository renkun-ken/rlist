#' Call a function with a list of arguments
#'
#' @param .data \code{list}. \code{vector} will be coreced to \code{list} before
#' being passed to \code{fun}.
#' @param fun The \code{function} to call
#' @param ... The additional parameters passed to \code{do.call}
#' @export
#' @examples
#' x <- lapply(1:3, function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3, function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.do(x, rbind)
list.do <- function(.data, fun, ...) {
  do.call(what = fun, args = as.list(.data), ...)
}

#' Bind all list elements by row
#'
#' The function binds all list elements by row. Each element of the list is expected
#' to be an atomic vector, \code{data.frame}, or \code{data.table}. If list elements
#' are also lists, the result can be a list-valued matrix. In this case,
#' \code{list.stack} may produce a better result.
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
#' The function binds all list elements by column. Each element of the list is expected
#' to be an atomic vector, \code{data.frame}, or \code{data.table} of the same length.
#' If list elements are also lists, the binding will flatten the lists and may produce
#' undesired results.
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
list.apply <- function(.data, .fun, ...) lapply(X = .data, FUN = .fun, ...)
