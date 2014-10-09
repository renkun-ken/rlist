#' Group a list by the value of an expression evaluated for each member.
#'
#' @param .data \code{list}
#' @param ... keys
#' @name list.group
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.group(x,type)
#' list.group(x,mean(unlist(score)))
#' }
list.group <- function(.data, ...) {
  args <- dots(...)
  keys <- list.map.internal(.data,args[[1L]],envir = parent.frame())
  unikeys <- unique(keys)
  names(unikeys) <- as.character(unikeys)
  lapply(unikeys,function(k) {
    .data[vapply(keys, identical, logical(1L), y=k)]
  })
}
