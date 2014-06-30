#' Sort a list by given expressions in order
#'
#' @param x The list to be sorted
#' @param ... Expressions to evaluate for sorting
#' @name list.sort
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.sort(x,type,desc(score$c2))
#' list.sort(x,min(score$c1,score$c2))
#' }
list.sort <- function(x,...) {
  args <- as.list(match.call(expand.dots = FALSE))$`...`
  envir <- new.env(FALSE,parent.frame(),.nsymbol)
  list2env(list.sort.functions,envir)
  cols <- lapply(args,function(arg) {
    if(is.null(arg)) stop("NULL condition")
    items <- list.map.internal(x,arg,envir = envir)
    unlist(items)
  })
  x[do.call(order,cols)]
}
