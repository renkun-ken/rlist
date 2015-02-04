#' Get or set the names of a list by expression
#' @param .data A \code{list} or \code{vector}
#' @param expr the expression whose value will be set as the name
#' for each list element. If missing then the names of the list will be
#' returned. If \code{NULL} then the names of the list will be removed.
#' @export
#' @examples
#' list.names(c(1,2,3))
#' list.names(c(a=1,b=2,c=3))
#' list.names(c(1,2,3),letters[.])
#' list.names(list(list(name='A',value=10),list(name='B',value=20)), name)
list.names <- function(.data, expr) {
  if (missing(expr)) 
    return(names(.data))
  expr <- substitute(expr)
  if (is.null(expr)) 
    return(setnames(.data, NULL))
  values <- list.map.internal(.data, expr, parent.frame())
  setnames(.data, values)
} 
