#' Stack all list elements to tabular data
#'
#' @param .data \code{list} of \code{vector}s, \code{list}s,
#'    \code{data.frame}s or \code{data.table}s.
#' @param ... additional parameters passed to \code{data.table::rbindlist}.
#' @param data.table \code{TRUE} to keep the result as \code{data.table}
#' @export
#' @importFrom data.table rbindlist
#' @examples
#' \dontrun{
#' x <- lapply(1:3, function(i) { list(a=i,b=i^2) })
#' list.stack(x)
#'
#' x <- lapply(1:3, function(i) { list(a=i,b=i^2,c=letters[i])})
#' list.stack(x)
#'
#' x <- lapply(1:3, function(i) { data.frame(a=i,b=i^2,c=letters[i]) })
#' list.stack(x)
#'
#' x <- lapply(1:3, function(i) { data.frame(a=c(i,i+1), b=c(i^2,i^2+1))})
#' list.stack(x)
#' }
list.stack <- function(.data, ..., data.table = FALSE) {
  dt <- data.table::rbindlist(.data, ...)
  if (!data.table)
    data.table::setDF(dt)
  dt
}
