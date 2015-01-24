#' Append a list
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... A \code{vector} or \code{list} to append after \code{x}
#' @name list.append
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.append(x,d=4,e=5)
#' list.append(x,d=4,f=c(2,3))
#' }
list.append <- function(.data,...) {
  if(is.list(.data)) {
    c(.data,list(...))
  } else {
    c(.data,...)
  }
}
