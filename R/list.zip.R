#' Combine multiple lists element-wisely.
#'
#' @param ... \code{list}s
#' @param use.argnames \code{logical}. Should the names of the
#'    arguments be used as the names of list items?
#' @param use.names \code{logical}. Should the names of the first
#'    argument be used as the zipped list?
#' @name list.zip
#' @export
#' @examples
#' \dontrun{
#' x <- list(1,2,3)
#' y <- list("x","y","z")
#' list.zip(num=x,sym=y)
#' }
list.zip <- function(...,use.argnames=TRUE,use.names=TRUE) {
  args <- list(...)
  if(use.argnames) args <- set_argnames(dots(...),args)
  results <- do.call(map,c(function(...) {
    list(...)
  },args))
  if(!use.names) names(results) <- NULL
  results
}
