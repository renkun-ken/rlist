#' Clean a list by a function
#'
#' @param .data \code{list}
#' @param fun A logical \code{function} for clean
#' @name list.clean
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=NULL,b=NULL,c=NULL,d=1,e=2)
#' list.clean(x)
#' }
list.clean <- function(.data,
  fun = function(x) is.null(x) || length(x) == 0L) {
  setmembers(.data,vapply(.data,fun,logical(1L)),NULL)
}
