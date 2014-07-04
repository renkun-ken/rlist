#' Call a function with a list of arguments as provided
#'
#' @param .data The list
#' @param fun The function to call
#' @param ... The additional parameters passed to \code{do.call}
#' @name list.do
#' @export
#' @examples
#' \dontrun{
#' x <- lapply(1:3,function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3,function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.do(x,rbind)
#' list.do(x,rbind)
#' as.list(1:10) %>>% list.map(x -> list.do(x,rnorm))
#' }
list.do <- function(.data,fun,...) {
  fun <- match.fun(fun)
  do.call(fun,as.list(.data),...)
}
