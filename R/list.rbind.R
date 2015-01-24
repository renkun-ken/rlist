#' Bind all list elements by row
#'
#' @param .data \code{list}
#' @export
#' @seealso \code{\link{list.cbind}}, \code{\link{list.stack}}
#' @examples
#' x <- lapply(1:3,function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3,function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.rbind(x)
#' list.rbind(df)
list.rbind <- function(.data) {
  list.do(.data, "rbind")
}
