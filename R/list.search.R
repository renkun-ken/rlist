#' Search a list recusively by a value
#'
#' @param .data \code{list}
#' @param expr a lambda expression with respect to value that returns
#'    a single-valued logical vector. In the expression, exact and fuzzy search
#'    functions are recommended.
#' @param classes a character vector of class names that restrict the search.
#'    By default, the range is unrestricted (\code{ANY}).
#' @param unlist \code{logical} Should the result be unlisted?
#' @param envir The environment to evaluate mapping function
#' @name list.search
#' @export
#' @examples
#' \dontrun{
#' # Exact search
#'
#' x <- list(p1 = list(type="A",score=c(c1=9)),
#'        p2 = list(type=c("A","B"),score=c(c1=8,c2=9)),
#'        p3 = list(type=c("B","C"),score=c(c1=9,c2=7)),
#'        p4 = list(type=c("B","C"),score=c(c1=8,c2=NA)))
#'
#' ## Search exact values
#' list.search(x, exact("A"))
#' list.search(x, exact(c("A","B")))
#' list.search(x, exact(c(9,7)))
#' list.search(x, exact(c(c1=9,c2=7)))
#'
#' ## Search all equal values
#' list.search(x, all(equal(9)))
#' list.search(x, all(equal(c(8,9))))
#' list.search(x, all(equal(c(8,9)),na.rm = TRUE))
#'
#' ## Search any equal values
#' list.search(x, any(equal(9)))
#' list.search(x, any(equal(c(8,9))))
#'
#' ## Search all/any included/excluded values
#' list.search(x, include(9))
#' list.search(x, all(include(c(9,10))))
#' list.search(x, any(include(c(9,10))))
#' list.search(x, all(!include(c(7,9,10))))
#'
#' # Fuzzy search
#'
#' data <- list(
#'   p1 = list(name="Ken",age=24),
#'   p2 = list(name="Kent",age=26),
#'   p3 = list(name="Sam",age=24),
#'   p4 = list(name="Keynes",age=30),
#'   p5 = list(name="Kwen",age=31)
#' )
#'
#' list.search(data, like("Ken",1), "character")
#' list.search(data, like("Man",2), "character")
#' list.search(data, !like("Man",2), "character")
#'
#' data <- list(
#'   p1 = list(name=c("Ken", "Ren"),age=24),
#'   p2 = list(name=c("Kent", "Potter"),age=26),
#'   p3 = list(name=c("Sam", "Lee"),age=24),
#'   p4 = list(name=c("Keynes", "Bond"),age=30),
#'   p5 = list(name=c("Kwen", "Hu"),age=31))
#'
#' list.search(data, all(like("Ken",1)), "character")
#' list.search(data, any(like("Ken",1)), "character")
#' list.search(data, all(!like("Ken",1)), "character")
#' list.search(data, any(!like("Ken",1)), "character")
#' }
list.search <- function(.data, expr, classes = "ANY", unlist = FALSE,
  envir = parent.frame()) {
  l <- lambda(substitute(expr))
  fun <- list.search.fun
  environment(fun) <- envir
  formals(fun) <- setnames(formals(fun),c(".data",".expr",l$symbols))
  results <- rapply(.data, fun, classes = classes,
    how = if(unlist) "unlist" else "list", .expr = l$expr)
  if(!unlist) {
    results <- list.clean(results,
      fun = is.null.or.empty, recursive = TRUE)
  }
  results
}

#' Comparer functions
#' @name comparers
#' @details
#' \code{exact}: test if two objects are exactly identical.
#'
#' \code{equal}: test if two atomic vectors of the same mode and length
#'    are equal.
#'
#' \code{include}: test if the values of an atomic vector are included
#'    by the other with the same mode, respectively.
#'
#' \code{like}: test if the distance between two atomic character vectors
#'    is no greater than a given value.
#' @param x target
#' @param y source
#' @param dist maximum distance
#' @param ... additional parameters: for \code{exact},
#'    passed to \code{identical}; for \code{like},
#'    passed to \code{stringdist::stringdist}.
#' @return \code{logical}
#' @export
exact <- function(x,y = get(".data", envir = parent.frame()),...) {
  identical(x,y,...)
}

#' @export
#' @rdname comparers
equal <- function(x,y = get(".data", envir = parent.frame())) {
  if(mode(x) == mode(y) && length(x) == length(y)) x == y
  else FALSE
}

#' @export
#' @rdname comparers
include <- function(x,y = get(".data", envir = parent.frame())) {
  if(mode(x) == mode(y)) x %in% y
  else FALSE
}

#' @export
#' @rdname comparers
like <- function(x, dist = 1L,
  y = get(".data", envir = parent.frame()), ...) {
    stringdist::stringdist(x,y,...) <= dist
}
