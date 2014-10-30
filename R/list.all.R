#' Examine if a condition is true for all list elements
#'
#' @param .data \code{list}
#' @param cond A logical lambda expression
#' @param na.rm logical. If true the function returns \code{NA} when \code{NA}
#' is encountered as \code{cond} is evaluated.
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.all(x,type=="B")
#' list.all(x,mean(unlist(score))>=6)
#' }
list.all <- function(.data, cond, na.rm = FALSE) {
  if(missing(.data)) return(all(na.rm = na.rm))
  if(is.empty(.data) || missing(cond)) return(all(.data, na.rm = na.rm))
  res <- list.first.internal(.data, substitute(!cond),
    parent.frame(), na.stop = !na.rm)
  !res$state
}
