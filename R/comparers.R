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
#' \code{pattern}: test if the values match a particular pattern of
#'    regular expression.
#'
#' \code{like}: test if the distance between two atomic character vectors
#'    is no greater than a given value.
#' @param x target
#' @param y source
#' @param dist maximum distance
#' @param ... additional parameters:
#'    for \code{exact}, passed to \code{identical};
#'    for \code{pattern}, passed to \code{grepl};
#'    for \code{like}, passed to \code{stringdist::stringdist}.
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
pattern <- function(x, y = get(".data", envir = parent.frame()), ...) {
  grepl(x, y, ...)
}

#' @export
#' @rdname comparers
like <- function(x, dist = 1L,
  y = get(".data", envir = parent.frame()), ...) {
    stringdist::stringdist(x,y,...) <= dist
}
