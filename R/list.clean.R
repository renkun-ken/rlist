#' Clean a list by a function
#'
#' @param .data A \code{list} or \code{vector}
#' @param fun A logical \code{function} for cleaning
#' @param recursive \code{logical}. Should the list be
#'    cleaned recursively?
#' @export
#' @examples
#' x <- list(a=NULL,b=NULL,c=NULL,d=1,e=2)
#' list.clean(x)
list.clean <- function(.data, fun = "is.null", recursive = FALSE) {
  if(recursive) {
    .data <- lapply(.data, function(.item) {
      if(is.list(.item)) list.clean(.item, fun, recursive = TRUE)
      else .item
    })
  }
  setmembers(.data, vapply(.data, fun, logical(1L)), NULL)
}
