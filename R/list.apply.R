#' Apply a function to each list element (\code{lapply})
#' @export
#' @param .data A \code{list} or \code{vector}
#' @param .fun \code{function}
#' @param ... Additional parameters passed to \code{FUN}.
list.apply <- function(.data,.fun,...)
  lapply(X = .data, FUN = .fun, ...)
