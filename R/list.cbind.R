#' Bind all list members by column
#'
#' @param .data \code{list}
#' @name list.cbind
#' @export
#' @examples
#' \dontrun{
#' x <- list(data.frame(i=1:5,x=rnorm(5)),
#'    data.frame(y=rnorm(5),z=rnorm(5)))
#' list.cbind(x)
#' }
list.cbind <- function(.data) {
  list.do(.data,"cbind")
}
