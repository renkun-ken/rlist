#' Subset a list
#'
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.subset(x, c('p1','p2'))
#' list.subset(x, grepl('^p', names(x)))
#' \dontrun{
#' list.subset(x, stringdist::stringdist(names(x), 'x1') <= 1)
#' }
list.subset <- `[` 
