#' Clean a list by a function
#'
#' @param .data \code{list}
#' @param fun The function for cleaning, by default \code{is.null}.
#' @name list.clean
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=NULL,b=NULL,c=NULL,d=1,e=2)
#' list.clean(x)
#' }
list.clean <- function(.data,
  fun = function(x) is.null(x) || (is.vector(x) && length(x) == 0L)) {
  `[<-`(.data,vapply(.data,fun,logical(1L)),NULL)
}
