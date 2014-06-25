#' Select members from a list
#'
#' @name list.select
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.select(x,1)
#' list.select(x,"a")
#' }
list.select <- `[`
