#' Search a list recusively by a value
#'
#' @param .data \code{list}
#' @param when \code{function}: \code{any} or \code{all}
#' @param how A logical comparer \code{function}.
#'
#'     Exact search comparers
#'
#'     \code{identical}, \code{unidentical},
#'     \code{equal}, \code{unequal}
#'     \code{include}, \code{exclude}
#'
#'     Fuzzy search comparers
#'
#'     \code{like(dist)}, \code{unlike(dist)} where
#'     \code{dist} is the maximum or minimum string distance.
#' @param what The value to search
#' @param ... Additional parameters passed to \code{fun}
#' @param na.rm \code{logical} Should \code{NA} be ignored?
#' @param classes A character vector of class names that restrict the search. By default, the search range is restrcited to the class of \code{value}, so that types are strictly distinguished, that is, searching numeric \code{1} does not include comparing integer \code{1L}. To broader the search range, add more values to \code{classes} such as \code{classes = c("numeric","integer")}.
#' @param unlist \code{logical} Should the result be unlisted?
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
#' ## Search identical values
#' list.search(x, any, identical, "A")
#' list.search(x, any, identical, c("A","B"))
#' list.search(x, any ,identical, c(10,8))
#' list.search(x, any, identical, c(c1=9,c2=7))
#'
#' ## Search all equal values
#' list.search(x, all, equal, 9)
#' list.search(x, all, equal, c(8,9))
#' list.search(x, all, equal, c(8,9), na.rm = TRUE)
#'
#' ## Search any equal values
#' list.search(x, any, equal, 9)
#' list.search(x, any, equal, c(8,9))
#'
#' ## Search all/any included/excluded values
#' list.search(x, all, include, 9)
#' list.search(x, all, include, c(9,10))
#' list.search(x, any, include, c(9,10))
#' list.search(x, all, exclude, c(7,9,10))
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
#' list.search(data, any, like(1), "Ken")
#'
#' data <- list(
#'   p1 = list(name=c("Ken", "Ren"),age=24),
#'   p2 = list(name=c("Kent", "Potter"),age=26),
#'   p3 = list(name=c("Sam", "Lee"),age=24),
#'   p4 = list(name=c("Keynes", "Bond"),age=30),
#'   p5 = list(name=c("Kwen", "Hu"),age=31))
#'
#' list.search(data, all, like(1), "Ken")
#' list.search(data, any, like(1), "Ken")
#' list.search(data, all, unlike(1), "Ken")
#' list.search(data, any, unlike(1), "Ken")
#' }
list.search <- function(.data, when, how, what, ...,
  na.rm = FALSE, classes = class(what), unlist = FALSE) {
  when <- match.fun(when)
  how <- match.fun(how)
  results <- rapply(.data, function(src) {
    q <- when(how(src,what,...), na.rm = na.rm)
    if(!is.na(q) && q) src
  },classes = classes, how = if(unlist) "unlist" else "list")
  if(!unlist) {
    results <- list.clean(results,
      fun = is.null.or.empty, recursive = TRUE)
  }
  results
}

#' Exact search functions
#' @name exact-search
#' @param x value
#' @param y data
#' @param ... additional parameters
#' @return \code{TRUE} or \code{FALSE}
#' @export
equal <- function(x,y) {
  if(length(x) != length(y)) return(FALSE)
  y == x
}

#' @export
#' @rdname exact-search
unequal <- function(x,y) {
  !equal(x,y)
}

#' @export
#' @rdname exact-search
unidentical <- function(x,y,...) {
  !identical(x,y,...)
}

#' @export
#' @rdname exact-search
include <- function(x,y) {
  y %in% x
}

#' @export
#' @rdname exact-search
exclude <- function(x,y) {
  !(y %in% x)
}


#' Fuzzy search functions
#' @name fuzzy-search
#' @param dist string distance
#' @param ... additional parameter passed to
#'      \code{stringdist} functions
#' @return a closure \code{function(src,value)}
#' @export
like <- function(dist,...) {
  function(src,value) {
    stringdist::stringdist(value,src,...) <= dist
  }
}

#' @export
#' @rdname fuzzy-search
unlike <- function(dist,...) {
  function(src,value) {
    stringdist::stringdist(value,src,...) >= dist
  }
}
