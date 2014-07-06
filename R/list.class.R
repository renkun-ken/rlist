#' Classify list members into unique cases evaluated by given expression.
#'
#' @param .data \code{list}
#' @param expr A lambda expression
#' @param ... Additional parameters passed to \code{unique}
#' @param sort.cases \code{logical}. if \code{TRUE} the cases will be sorted in ascending order.
#' @name list.class
#' @export
#' @examples
#' \dontrun{
#' x <-
#'   list(
#'     p1=list(name="Ken",age=24,
#'       interest=c("reading","music","movies"),
#'       lang=list(r=2,csharp=4,python=3)),
#'     p2=list(name="James",age=25,
#'       interest=c("sports","music"),
#'       lang=list(r=3,java=2,cpp=5)),
#'     p3=list(name="Penny",age=24,
#'       interest=c("movies","reading"),
#'       lang=list(r=1,cpp=4,python=2)))
#' list.class(x,interest)
#' list.class(x,names(lang))
#' }
list.class <- function(.data,expr,...,sort.cases=TRUE) {
  values <- list.map.internal(.data,substitute(expr))
  cases <- unique(unlist(values,use.names = FALSE),...)
  names(cases) <- cases
  if(sort.cases)  cases <- sort(cases)
  lapply(cases,function(case) {
    .data[vapply(values,function(vi) case %in% vi,logical(1L))]
  })
}
