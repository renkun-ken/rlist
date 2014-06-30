#' Return the order of each member in a list by expression
#'
#' @param x The
#' @param ... Expressions to evaluate for ordering
#' @param keep.names Whether to keep the names of \code{x} in the result
#' @name list.order
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.order(x,type,desc(score$c2))
#' list.order(x,min(score$c1,score$c2))
#' list.order(x,min(score$c1,score$c2),keep.names=TRUE)
#' }
list.order <- function(x,...,keep.names=FALSE) {
  args <- as.list(match.call(expand.dots = FALSE))$`...`
  envir <- new.env(FALSE,parent.frame(),.nsymbol)
  list2env(list.sort.functions,envir)
  cols <- lapply(args,function(arg) {
    if(is.null(arg)) stop("NULL condition")
    items <- list.map.internal(x,arg,envir = envir)
    unlist(items)
  })
  result <- do.call(order,cols)
  if(keep.names) names(result) <- names(x)
  result
}
