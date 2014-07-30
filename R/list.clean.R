#' Clean a list by a function
#'
#' @param .data \code{list}
#' @param fun A logical \code{function} for clean
#' @param recursive \code{logical}. Should the list be
#'    cleaned recursively?
#' @name list.clean
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=NULL,b=NULL,c=NULL,d=1,e=2)
#' list.clean(x)
#' }
list.clean <- function(.data, fun = is.null, recursive = FALSE) {
  if(recursive) {
    .data <- lapply(.data, function(.item) {
      if(is.list(.item)) list.clean(.item, fun, TRUE)
      else .item
    })
  }
  setmembers(.data,vapply(.data,fun,logical(1L)),NULL)
}
