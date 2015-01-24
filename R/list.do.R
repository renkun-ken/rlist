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
