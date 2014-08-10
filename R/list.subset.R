#' Subset a list
#'
#' @name list.subset
#' @param x list
#' @param value names or indices
#' @param pattern \code{TRUE} if \code{value} indicates a regular
#'    expression pattern.
#' @param dist \code{integer} that indicates the maximal string distance to
#'    \code{value}.
#' @param ... additional parameters.
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.subset(x, c("p1","p2"))
#' list.subset(x, "^p", pattern = TRUE)
#' list.subset(x, "x1", dist = 1)
#' }
list.subset <- function(x, value, pattern = FALSE, dist = NA_integer_, ...) {
  if(pattern)
    x[grepl(value, names(x), ...)]
  else if(!is.na(dist))
    x[stringdist::stringdist(value,names(x),...) <= dist]
  else
    x[value,...]
}
