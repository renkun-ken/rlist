#' Search a list recusively by a value
#'
#' @param .data \code{list}
#' @param value The value to search
#' @param fun A logical \code{function} to compare value and list values
#' @param ... Additional parameters passed to \code{fun}
#' @param classes A character vector of class names that restrict the search
#' @name list.search
#' @export
#' @examples
#' \dontrun{
#' # Exact search
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.search(x, 9)
#'
#' # Case search
#' data <- list(
#'   p1 = list(x=c("A","B"),y=c("A","C")),
#'   p2 = list(q=c("B","C"),p=c("A","C","B")),
#'   p3 = list(m=list(c("A","B"),c("B","C")),n=c("A"))
#' )
#'
#' list.search(data,"A","%in%")
#'
#' # Fuzzy search with \code{stringdist} package
#' data <- list(
#'   p1 = list(name="Ken",age=24),
#'   p2 = list(name="Kent",age=26),
#'   p3 = list(name="Sam",age=24),
#'   p4 = list(name="Keynes",age=30),
#'   p5 = list(name="Kwen",age=31)
#' )
#'
#' list.search(data,"Ken",stringdist::ain, maxDist=1)
#' list.search(data,"Ken",function(x,y) stringdist::stringdist(x,y) < 1)
#' }
list.search <- function(.data, value, fun = identical, ...,
  classes = class(value), unlist = FALSE) {
  fun <- match.fun(fun)
  results <- rapply(.data,function(obj) {
    if(fun(value,obj,...)) obj
  },classes=classes,how=if(unlist) "unlist" else "list")
  if(!unlist) {
    results <- list.clean(results,
      fun = is.null.or.empty, recursive = TRUE)
  }
  results
}
