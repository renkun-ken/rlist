#' Get or set the names of a list by expression
#' @param .data the list
#' @param expr the expression whose value will be set as the name
#' for each list element. If missing then the names of the list will be
#' returned
#' @export
list.names <- function(.data, expr) {
  if(missing(expr)) return(names(.data))
  values <- list.map.internal(.data,substitute(expr),envir = parent.frame())
  setnames(.data, values)
}
