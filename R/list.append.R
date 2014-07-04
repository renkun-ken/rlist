#' Append a list
#'
#' @param .data The original list
#' @param ... A vector or list that is to be appended after \code{x}
#' @name list.append
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.append(x,d=4,e=5)
#' list.append(x,d=4,f=c(2,3))
#' }
list.append <- function(.data,...) {
  if(is.vector(.data)) {
    c(.data,...)
  } else {
    c(.data,list(...))
  }
}
