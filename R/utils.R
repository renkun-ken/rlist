#' Try to get the value of a symbol if exists or return a default value
#' @details
#' By default, the symbol is examined in \code{envir} without inheritance,
#' that is, if the symbol does not exist in \code{envir} the default value
#' \code{def} will be returned.
#' @param symbol the symbol to examine
#' @param def the default value if the symbol does not exist
#' @param ... additional parameters passed to \code{exists} and \code{get}
#' @param envir the environment to examine whether the symbol exists
#' and get the symbol
#' @export
#' @examples
#' x <- list(a=c(x=1,y=2),b=c(x=2,p=3))
#' list.map(x, tryGet(y,0))
tryGet <- function(symbol, def = NULL, ..., envir = parent.frame()) {
  symbol <- substitute(symbol)
  if (is.symbol(symbol)) symbol <- as.character(symbol)
  if (is.character(symbol)) {
    if (exists(symbol, inherits = FALSE, ..., envir = envir))
      get(symbol, inherits = FALSE, ..., envir = envir) else def
  } else stop("symbol must be a name or character", call. = FALSE)
}

#' Try to evaluate an expression and return a default value if
#' an error occurs or otherwise return its value.
#' @param expr the expression to evaluate
#' @param def the default value if an error occurs in the evaluation
#' of \code{expr}
#' @export
#' @examples
#' x <- list(a=c(x=1,y=2),b=c(x=2,p=3))
#' list.map(x, tryEval(x+y, NA))
tryEval <- function(expr, def = NULL) {
  x <- try(expr, silent = TRUE)
  if (is.error(x)) def else x
}
