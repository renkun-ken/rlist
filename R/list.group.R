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

#' Ungroup a list by taking out second-level elements
#'
#' This functon reverses the grouping operation by taking out
#' second-level elements of a nested list and removing the labels
#' of the first-level elements. For example, a list may be created
#' from paged data, that is, its first-level elements only indicate
#' the page container. To unpage the list, the first-level elements
#' must be removed and their inner elements should be taken out to
#' to the first level.
#' @param .data \code{list}
#' @param level {integer} to indicate to which level of list elements
#' should be ungroupped to the first level.
#' @param ... Preserved use of parameter passing
#' @param group.names \code{logical}. Should the group names be
#' preserved?
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
#'
#' x <- list(a = list(a1 = list(x=list(x1=2,x2=3),y=list(y1=1,y2=3))),
#'        b = list(b1 = list(x=list(x1=2,x2=6),y=list(y1=3,y2=2))))
#' list.ungroup(x, level = 1)
#' list.ungroup(x, level = 2)
#' list.ungroup(x, level = 2, group.names = TRUE)
list.ungroup <- function(.data, level = 1L, ..., group.names = FALSE, sort.names = FALSE) {
  result <- .data
  for(i in seq_len(level)) {
    if(!group.names) names(result) <- NULL
    result <- unlist(result, recursive = FALSE)
  }
  result.names <- names(result)
  if (sort.names && !is.null(result.names)) {
    result[sort(result.names)]
  } else {
    result
  }
}

#' Classify list elments into unique but non-exclusive cases
#'
#' In non-tabular data, a certain field may take multiple values in a
#' collection non-exclusively. To classify these elements into different
#' cases, this function detects all possible cases and for each case all
#' elements are examined whether to belong to that case.
#' @param .data A \code{list} or \code{vector}
#' @param ... keys
#' @param sorted \code{TRUE} to sort the group keys. Ignored when the key has
#' multiple entries.
#' @export
#' @return a list of possible cases each of which contains elements belonging to
#' the case non-exclusively.
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

#' Get all unique cases of a list field by expression
#'
#' @param .data A \code{list} or \code{vector}
#' @param expr A lambda expression. The function will returns all cases
#' of the elements if \code{expr} is missing.
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
#'
#' foo <- list(x = LETTERS[1:3], y = LETTERS[3:5])
#' list.cases(foo)
list.cases <- function(.data, expr, simplify = TRUE, sorted = TRUE) {
  expr <- if(missing(expr)) quote(.) else substitute(expr)
  values <- list.map.internal(.data, expr, parent.frame())
  if (simplify && all(vapply(values, is.atomic, logical(1L)))) {
    values <- c(values, recursive = TRUE)
  }
  cases <- unique(values)
  if (sorted && is.atomic(cases))
    cases <- sort(cases)
  cases
}
