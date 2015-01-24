#' Group a list by the value of an expression evaluated for each member.
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... keys
#' @param sorted \code{TRUE} to sort the group keys. Ignored when the key has
#' multiple entries.
#' @seealso \code{\link{list.group}}
#' @export
#' @examples
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.group(x, type)
#' list.group(x, mean(unlist(score)))
list.group <- function(.data, ..., sorted = TRUE) {
  list.group.internal(.data, dots(...), parent.frame(),
    compare = "identical", sorted = sorted)
}

