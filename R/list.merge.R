#' Merge a number of named lists in sequential order
#'
#' The function merges a number of lists in sequential order
#' by \code{modifyList}, that is, the later list always
#' modifies the former list and form a merged list, and the
#' resulted list is again being merged with the next list.
#' The process is repeated until all lists in \code{...} or
#' \code{list} are exausted.
#'
#' @details
#' List merging is usually useful in the merging of program
#' settings or configuraion with multiple versions across time,
#' or multiple administrative levels. For example, a program
#' settings may have an initial version in which most keys are
#' defined and specified. In later versions, partial modifications
#' are recorded. In this case, list merging can be useful to merge
#' all versions of settings in release order of these versions. The
#' result is an fully updated settings with all later modifications
#' applied.
#' @param ... named lists
#' @importFrom utils modifyList
#' @export
#' @examples
#' l1 <- list(a=1,b=list(x=1,y=1))
#' l2 <- list(a=2,b=list(z=2))
#' l3 <- list(a=2,b=list(x=3))
#' list.merge(l1,l2,l3)
list.merge <- function(...) {
  lists <- list(...)
  if (any(vapply(lists, function(x) is.null(names(x)), logical(1L))))
    stop("All arguments must be named list", call. = FALSE)
  reduce(modifyList, lists, list())
}
