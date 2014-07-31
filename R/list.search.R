#' Search a list recusively by a value
#'
#' @param .data \code{list}
#' @param fun A logical comparer \code{function}.
#'
#'     Exact search comparers
#'
#'     \code{identical}, \code{allEqual}, \code{anyEqual},
#'     \code{unidentical}, \code{allUnequal}, \code{anyUnequal}
#'
#'     Fuzzy search comparers
#'
#'     \code{anyLike(dist)}, \code{allLike(dist)},
#'     \code{anyUnlike(dist)}, \code{allUnlike(dist)} where
#'     \code{dist} is the maximum or minimum string distance.
#' @param value The value to search
#' @param ... Additional parameters passed to \code{fun}
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
#' list.search(x, identical, "A")
#' list.search(x, identical, c("A","B"))
#' list.search(x, identical, c(10,8))
#' list.search(x, identical, c(c1=10,c2=8))
#'
#' ## Search all equal values
#' list.search(x, allEqual, 9)
#' list.search(x, allEqual, c(10,8))
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
#' list.search(data,anyLike(1), "Ken")
#' list.search(data,allLike(1), "Ken")
#'
#' data <- list(
#'   p1 = list(name=c("Ken", "Ren"),age=24),
#'   p2 = list(name=c("Kent", "Potter"),age=26),
#'   p3 = list(name=c("Sam", "Lee"),age=24),
#'   p4 = list(name=c("Keynes", "Bond"),age=30),
#'   p5 = list(name=c("Kwen", "Hu"),age=31))
#'
#' list.search(data, allLike(1), "Ken")
#' list.search(data, anyLike(1), "Ken")
#' list.search(data, allUnlike(1), "Ken")
#' list.search(data, anyUnlike(1), "Ken")
#' }
list.search <- function(.data, fun, value, ...,
  classes = class(value), unlist = FALSE) {
  fun <- match.fun(fun)
  results <- rapply(.data,
    function(src) if(fun(src,value,...)) src,
    classes = classes,
    how = if(unlist) "unlist" else "list")
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
allEqual <- function(x,y,...) {
  if(length(x) != length(y)) return(FALSE)
  equal <- all(x==y,...)
  if(is.na(equal)) return(FALSE)
  equal
}

#' @export
#' @rdname exact-search
anyEqual <- function(x,y,...) {
  any(x %in% y,...)
}

#' @export
#' @rdname exact-search
allUnequal <- function(x,y,...) {
  !anyEqual(x,y,...)
}

#' @export
#' @rdname exact-search
anyUnequal <- function(x,y,...) {
  !allEqual(x,y,...)
}

#' @export
#' @rdname exact-search
unidentical <- function(x,y,...) {
  !identical(x,y,...)
}



#' Fuzzy searching functions
#' @name fuzzy-search
#' @param dist string distance
#' @param ... Additional parameter passed to \code{stringdist} functions
#' @return a closure \code{function(src,value)}
#' @export
allLike <- function(dist,...) {
  function(src,value) {
    all(stringdist::stringdist(value,src,...) <= dist)
  }
}

#' @export
#' @rdname fuzzy-search
allUnlike <- function(dist,...) {
  function(src,value) {
    all(stringdist::stringdist(value,src,...) >= dist)
  }
}

#' @export
#' @rdname fuzzy-search
anyLike <- function(dist,...) {
  function(src,value) {
    stringdist::ain(value,src,maxDist=dist,...)
  }
}

#' @export
#' @rdname fuzzy-search
anyUnlike <- function(dist,...) {
  function(src,value) {
    any(stringdist::stringdist(value,src,...) >= dist)
  }
}
