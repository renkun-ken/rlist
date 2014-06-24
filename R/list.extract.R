#' Extract members from a list
#'
#' @param x The original list
#' @param val A vector or list that is to be appended after \code{x}
#' @name list.append
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.extract(x,1)
#' list.extract(x,c("a","c"))
#' }
list.extract <- `[`
