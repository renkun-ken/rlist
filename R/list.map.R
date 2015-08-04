#' Map each element in a list or vector by an expression.
#'
#' @param .data a \code{list} or \code{vector}
#' @param expr A lambda expression
#' @return A \code{list} in which each element is mapped by \code{expr} in \code{.data}
#' @export
#' @seealso \code{\link{list.mapv}}
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.map(x, type)
#' list.map(x, min(score$c1,score$c2))
list.map <- function(.data, expr) {
  list.map.internal(.data, substitute(expr), parent.frame())
}

#' Map each member of a list by an expression to a vector.
#'
#' @param .data a \code{list} or \code{vector}
#' @param expr a lambda expression
#' @param as the mode to corece. Missing to \code{unlist}
#' the mapped results.
#' @param use.names Should the names of the results be preserved?
#' @return A \code{vector} in which each element is mapped by \code{expr} in \code{.data}
#' @export
#' @seealso \code{\link{list.map}}
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.mapv(x, type)
#' list.mapv(x, min(score$c1,score$c2))
list.mapv <- function(.data, expr, as, use.names = TRUE) {
  res <- list.map.internal(.data, substitute(expr), parent.frame())
  if (missing(as))
    unlist(res, use.names = use.names) else {
    res <- as.vector(res, as)
    if (use.names && !is.null(nm <- names(.data)))
      names(res) <- nm
    res
  }
}

#' Map multiple lists with an expression
#'
#' @param expr An implicit lambda expression where only \code{.i} and
#'    \code{.name} are defined.
#' @param ... Named arguments of lists with equal length. The names of the
#'    lists are available as symbols that represent the element for each list.
#' @name list.maps
#' @export
#' @examples
#' \dontrun{
#' l1 <- list(p1=list(x=1,y=2), p2=list(x=3,y=4), p3=list(x=1,y=3))
#' l2 <- list(2,3,5)
#' list.maps(a$x*b+a$y,a=l1,b=l2)
#' list.maps(..1$x*..2+..1$y,l1,l2)
#' }
list.maps <- function(expr, ...) {
  expr <- substitute(expr)
  envir <- parent.frame()
  lists <- list(...)
  if (is.empty(lists))
    return(list())
  list1 <- lists[[1L]]
  xnames <- getnames(list1, character(1L))
  fun <- with(envir, function(..., .expr) eval(.expr, list(...)))
  args <- c(lists, list(.i = seq_along(list1), .name = xnames, .expr = list(expr)))
  map(fun, args)
}

#' Iterate a list by evaluating an expression on
#' each list element
#'
#' @param .data \code{list}
#' @param expr A lambda expression
#' @name list.iter
#' @export
#' @return \code{invisible(.data)}
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.iter(x,cat(paste(type,'\n')))
#' list.iter(x,cat(str(.)))
list.iter <- function(.data, expr) {
  list.map.internal(.data, substitute(expr), parent.frame())
  invisible(.data)
}
