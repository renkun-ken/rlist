#' Merge a series of named lists
#' @param ... named lists
#' @export
#' @examples
#' \dontrun{
#' l1 <- list(a=1,b=list(x=1,y=1))
#' l2 <- list(a=2,b=list(z=2))
#' l3 <- list(a=2,b=list(x=3))
#' list.merge(l1,l2,l3)
#' }
list.merge <- function(...) {
  lists <- list(...)
  if(any(vapply(lists, function(x) is.null(names(x)), logical(1L))))
    stop("All arguments must be named list", call. = FALSE)
  reduce(modifyList, lists, list())
}
