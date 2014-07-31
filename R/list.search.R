#' Search a list recusively by a value
#'
#' @param .data \code{list}
#' @param value The value to search
#' @param fun A logical \code{function} to compare value and list values
#' @param ... Additional parameters passed to \code{fun}
#' @param classes A character vector of class names that restrict the search. By default, the search range is restrcited to the class of \code{value}, so that types are strictly distinguished, that is, searching numeric \code{1} does not include comparing integer \code{1L}. To broader the search range, add more values to \code{classes} such as \code{classes = c("numeric","integer")}.
#' @param unlist \code{logical} Should the result be unlisted?
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
#' list.search(data,"Ken",anyLike(1))
#' list.search(data,"Ken",allLike(1))
#'
#' x <- list(
#'   p1 = list(name=c("Ken", "Ren"),age=24),
#'   p2 = list(name=c("Kent", "Potter"),age=26),
#'   p3 = list(name=c("Sam", "Lee"),age=24),
#'   p4 = list(name=c("Keynes", "Bond"),age=30),
#'   p5 = list(name=c("Kwen", "Hu"),age=31))
#' list.search(data,"Ken",allUnlike(1))
#' list.search(data,"Ken",anyUnlike(2))
#' }
list.search <- function(.data, value, fun = equal, ...,
  classes = class(value), unlist = FALSE) {
  fun <- match.fun(fun)
  results <- rapply(.data,function(obj) {
    if(fun(value,obj,...)) obj
  },classes = classes,how = if(unlist) "unlist" else "list")
  if(!unlist) {
    results <- list.clean(results,
      fun = is.null.or.empty, recursive = TRUE)
  }
  results
}

#' Fuzzy searching functions
#' @name fuzzy-searching
#' @param dist string distance
#' @param ... Additional parameter passed to \code{stringdist} functions
#' @return a closure \code{function(x,y)}
#' @export
allLike <- function(dist,...) {
  function(x,y) {
    all(stringdist::stringdist(x,y,...) <= dist)
  }
}

#' @export
#' @rdname fuzzy-searching
allUnlike <- function(dist,...) {
  function(x,y) {
    all(stringdist::stringdist(x,y,...) >= dist)
  }
}

#' @export
#' @rdname fuzzy-searching
anyLike <- function(dist,...) {
  function(x,y) {
    stringdist::ain(x,y,maxDist=dist,...)
  }
}

#' @export
#' @rdname fuzzy-searching
anyUnlike <- function(dist,...) {
  function(x,y) {
    any(stringdist::stringdist(x,y,...) >= dist)
  }
}
