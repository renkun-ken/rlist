#' Stack all list members to construct a \code{data.frame}
#'
#' @param .data \code{list}
#' @name list.stack
#' @export
#' @examples
#' \dontrun{
#' x <- lapply(1:3,function(i) { list(a=i,b=i^2,c=letters[i])})
#' list.stack(x)
#' }
list.stack <- function(.data) {
  dt <- data.table::rbindlist(.data)
  class(dt) <- "data.frame"
  dt
}
