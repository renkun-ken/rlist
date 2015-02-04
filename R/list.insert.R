#' Insert a series of lists at the given index
#'
#' @param .data A \code{list} or \code{vector}
#' @param index The index at which the lists are inserted
#' @param ... A group of lists
#' @export
#' @seealso \code{\link{list.append}}, \code{\link{list.prepend}}
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' list.insert(x, 2, p2.1 = list(type='B',score=list(c1=8,c2=9)))
#' }
list.insert <- function(.data, index, ...) {
  values <- if (is.list(.data)) 
    list(...) else c(..., recursive = FALSE)
  n <- length(.data)
  if (index < -n) 
    stop("Invalid index")
  if (index < 0L) 
    index <- n + index + 1L
  c(.data[0L:max(0L, index - 1L)], values, if (index <= n) .data[index:length(.data)] else NULL)
}

#' Append elements to a list
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... A \code{vector} or \code{list} to append after \code{x}
#' @export
#' @seealso \code{\link{list.prepend}}, \code{\link{list.insert}}
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.append(x,d=4,e=5)
#' list.append(x,d=4,f=c(2,3))
#' }
list.append <- function(.data, ...) {
  if (is.list(.data)) {
    c(.data, list(...))
  } else {
    c(.data, ..., recursive = FALSE)
  }
}

#' Prepend elements to a list
#'
#' @param .data A \code{list} or \code{vector}
#' @param ... The \code{vector} or \code{list} to prepend before \code{x}
#' @export
#' @seealso \code{\link{list.append}}, \code{\link{list.insert}}
#' @examples
#' x <- list(a=1,b=2,c=3)
#' list.prepend(x, d=4, e=5)
#' list.prepend(x, d=4, f=c(2,3))
list.prepend <- function(.data, ...) {
  if (is.list(.data)) {
    c(list(...), .data)
  } else {
    c(..., .data, recursive = FALSE)
  }
} 
