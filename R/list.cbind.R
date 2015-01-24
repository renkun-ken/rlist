#' Bind all list elements by column
#'
#' @param .data \code{list}
#' @export
#' @seealso \code{\link{list.cbind}}, \code{\link{list.stack}}
#' @examples
#' x <- list(data.frame(i=1:5,x=rnorm(5)),
#'    data.frame(y=rnorm(5),z=rnorm(5)))
#' list.cbind(x)
list.cbind <- function(.data) {
  list.do(.data, "cbind")
}
