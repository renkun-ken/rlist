#' Clean a list by a function
#'
#' @param x The list
#' @param fun The function for cleaning, by default \code{is.null}.
#' @name list.clean
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=NULL,b=NULL,c=NULL,d=1,e=2)
#' list.clean(x)
#' }
list.clean <- function(x,fun=is.null) {
  x[vapply(x,fun,logical(1L))] <- NULL
  x
}
