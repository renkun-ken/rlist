#' Merge a series of lists
#' @param ... A group of lists
#' @name list.merge
#' @export
#' @examples
#' \dontrun{
#' l1 <- list(a=1,b=list(x=1,y=1))
#' l2 <- list(a=2,b=list(z=2))
#' l3 <- list(a=2,b=list(x=3))
#' list.merge(l1,l2,l3)
#' }
list.merge <- function(...) {
  updates <- list(...)
  result <- list()
  for (update in updates) {
    result <- modifyList(result, update)
  }
  result
}

