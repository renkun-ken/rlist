#' Update a list by appending or modifying its elements.
#'
#' The function updates each element of a list by evaluating
#' a group of expressions in the scope of the element. If the
#' name of an expression alreadys exists in an list element,
#' then the field with the name will be updated. Otherwise,
#' the value with the name will be appended to the list
#' element. The functionality is essentially done by
#' \code{modifyList}.
#'
#' @param .data \code{list}
#' @param ... A group of labmda expressions
#' @param keep.null Should \code{NULL} values be preserved
#'    for \code{modifyList}
#' @importFrom utils modifyList
#' @export
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.update(x, high=max(score$c1,score$c2), low=min(score$c1,score$c2))
#' list.update(x, exams=length(score))
#' list.update(x, grade=ifelse(type=='A', score$c1, score$c2))
#' list.update(x, score=list(min=0, max=10))
list.update <- function(.data, ..., keep.null = FALSE) {
  items <- lapply(dots(...), list.map.internal, .data = .data, envir = parent.frame())
  map(function(.data, ...) modifyList(.data, list(...), keep.null = keep.null),
    c(list(.data), items))
}
