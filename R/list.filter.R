#' Filter a list or vector by a series of conditions
#'
#' The function recursively filters the data by a given series of
#' conditions. The filter can be a single condition or multiple
#' conditions. \code{.data} will be filtered by the first condition;
#' then the results will be filtered by the second condition, if any;
#' then the results will be filtered by the third, if any, etc. The
#' results only contain elements satisfying all conditions specified
#' in \code{...}.
#' @param .data A \code{list} or \code{vector}
#' @param ... logical conditions
#' @export
#' @return elements in \code{.data} satisfying all conditions
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.filter(x, type=='B')
#' list.filter(x, min(score$c1, score$c2) >= 8)
#' list.filter(x, type=='B', score$c2 >= 8)
list.filter <- function(.data, ...) {
  conds <- dots(...)
  envir <- parent.frame()
  reduce(function(data, cond) {
    data[which(list.is.internal(data, cond, envir))]
  }, conds, .data)
}
