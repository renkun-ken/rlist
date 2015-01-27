#' Ungroup a list
#'
#' @param .data \code{list}
#' @param sort.names \code{logical}. Should the members be sorted
#' after ungrouping?
#' @seealso \code{\link{list.group}}
#' @export
#' @examples
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' xg <- list.group(x, type)
#' list.ungroup(xg)
list.ungroup <- function(.data, sort.names = FALSE) {
  names(.data) <- NULL
  result <- unlist(.data, recursive = FALSE)
  result.names <- names(result)
  if (sort.names && !is.null(result.names)) {
    result[sort(result.names)]
  } else {
    result
  }
}
