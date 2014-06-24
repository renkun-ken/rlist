#' Append a list
#'
#' @param x The original list
#' @param val A vector or list that is to be appended after \code{x}
#' @name list.append
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.append(x,c(d=4,e=5))
#' list.append(x,list(d=4,f=c(2,3)))
#' }
list.append <- function(x,val) {
  c(x,val)
}
