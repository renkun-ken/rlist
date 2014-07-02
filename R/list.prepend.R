#' Prepend a list
#'
#' @param x The original list
#' @param ... A vector or list that is to be prepended before \code{x}
#' @name list.prepend
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.prepend(x,d=4,e=5)
#' list.prepend(x,d=4,f=c(2,3))
#' }
list.prepend <- function(x,...) {
  c(list(...),x)
}
