#' Count the number of members that meet given condition
#'
#' @param .data A \code{list} or \code{vector}
#' @param cond A logical lambda expression.
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.count(x,type=='B')
#' list.count(x,min(unlist(score)) >= 9)
list.count <- function(.data, cond) {
  if (missing(cond)) 
    return(length(.data))
  length(which(list.is.internal(.data, substitute(cond), parent.frame())))
} 
