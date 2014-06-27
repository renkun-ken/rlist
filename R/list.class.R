#' Classify list members into unique cases evaluated by given expression.
#'
#' @param x The list to be classified
#' @param expr An expression that determines cases
#' @param sort.cases logical. if TRUE the cases will be sorted in ascending order.
#' @param keep.case.names Whether to keep the names of the cases
#' @param keep.item.names Whether to keep the names of the items in the result
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
list.class <- function(x,expr,
  sort.cases=TRUE,keep.case.names=TRUE,keep.item.names=TRUE) {
  expr <- substitute(expr)
  values <- list.map(x,eval(expr))
  cases <- unique(unlist(values,use.names = FALSE))
  if(keep.case.names) names(cases) <- cases
  if(sort.cases)  cases <- sort(cases)
  classes <- lapply(cases,function(case) {
    selector <- vapply(values,function(vi) case %in% vi,logical(1))
    indices <- which(selector)
    result <- x[indices]
    if(!keep.item.names) names(result) <- NULL
    result
  })
  classes
}
