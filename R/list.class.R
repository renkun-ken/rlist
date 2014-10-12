#' Classify list members into unique cases evaluated by given expression.
#'
#' @param .data \code{list}
#' @param ... keys
#' @param sorted \code{TRUE} to sort the group keys. Ignored when the key has
#' multiple entries.
#' @name list.class
#' @export
#' @examples
#' \dontrun{
#' x <-
#'   list(
#'     p1=list(name="Ken",age=24,
#'       interest=c("reading","music","movies"),
#'       lang=list(r=2,csharp=4,python=3)),
#'     p2=list(name="James",age=25,
#'       interest=c("sports","music"),
#'       lang=list(r=3,java=2,cpp=5)),
#'     p3=list(name="Penny",age=24,
#'       interest=c("movies","reading"),
#'       lang=list(r=1,cpp=4,python=2)))
#' list.class(x,interest)
#' list.class(x,names(lang))
#' }
list.class <- function(.data, ..., sorted = TRUE) {
  list.group.internal(.data, dots(...),
    proc = "unlist", compare = "contains", sorted = sorted, envir = parent.frame())
}
