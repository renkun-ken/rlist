#' Divide list/vector elements into exclusive groups
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... One or more expressions in the scope of each element to evaluate
#' as keys
#' @param sorted \code{TRUE} to sort the group keys. Ignored when the key has
#' multiple entries.
#' @seealso \code{\link{list.ungroup}}
#' @export
#' @return A list of group elements each contain all the elements in \code{.data}
#' belonging to the group
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.group(x, type)
#' list.group(x, mean(unlist(score)))
list.group <- function(.data, ..., sorted = TRUE) {
  list.group.internal(.data, dots(...), parent.frame(), compare = "identical",
    sorted = sorted)
}

#' Ungroup a list
#'
#' @param .data \code{list}
#' @param sort.names \code{logical}. Should the members be sorted
#' after ungrouping?
#' @seealso \code{\link{list.group}}
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' xg <- list.group(x, type)
#' list.ungroup(xg)
list.ungroup <- function(.data, sort.names = FALSE) {
  names(.data) <- NULL
  result <- unlist(.data, recursive = FALSE)
  result.names <- names(result)
  if (sort.names && !is.null(result.names)) {
    result[sort(result.names)]
  } else {
    result
  }
}

#' Classify list elments into unique but non-exclusive cases
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... keys
#' @param sorted \code{TRUE} to sort the group keys. Ignored when the key has
#' multiple entries.
#' @export
#' @examples
#' x <-
#'   list(
#'     p1=list(name='Ken',age=24,
#'       interest=c('reading','music','movies'),
#'       lang=list(r=2,csharp=4,python=3)),
#'     p2=list(name='James',age=25,
#'       interest=c('sports','music'),
#'       lang=list(r=3,java=2,cpp=5)),
#'     p3=list(name='Penny',age=24,
#'       interest=c('movies','reading'),
#'       lang=list(r=1,cpp=4,python=2)))
#' list.class(x,interest)
#' list.class(x,names(lang))
list.class <- function(.data, ..., sorted = TRUE) {
  list.group.internal(.data, dots(...), parent.frame(), proc = "unlist", compare = "contains",
    sorted = sorted)
}

#' Get all unique cases by expression for a list
#'
#' @param .data A \code{list} or \code{vector}
#' @param expr expression
#' @param simplify \code{logical}. Should atomic vectors be simplified
#'    by \code{unlist}?
#' @param sorted \code{logical}. Should the cases be sorted in ascending order?
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.cases(x,type)
#' list.cases(x,mean(unlist(score)))
list.cases <- function(.data, expr, simplify = TRUE, sorted = TRUE) {
  values <- list.map.internal(.data, substitute(expr), parent.frame())
  if (simplify && all(vapply(values, is.atomic, logical(1L)))) {
    values <- c(values, recursive = TRUE)
  }
  cases <- unique(values)
  if (sorted && is.atomic(cases))
    cases <- sort(cases)
  cases
}
