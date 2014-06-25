#' Extract a member from a list
#'
#' @name list.extract
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.extract(x,1)
#' list.extract(x,"a")
#' }
list.extract <- `[[`
