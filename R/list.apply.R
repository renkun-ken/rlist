#' Apply a function to each list member (\code{lapply})
#' @export
#' @param .data \code{list}
#' @param .fun \code{function}
#' @param ... Additional parameters passed to \code{FUN}.
list.apply <- function(.data,.fun,...)
  lapply(X = .data, FUN = .fun, ...)
