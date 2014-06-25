#' Bind all list members by row
#'
#' @param x The list
#' @name list.rbind
#' @export
#' @examples
#' \dontrun{
#' x <- lapply(1:3,function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3,function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.rbind(x)
#' list.rbind(df)
#' }
list.rbind <- function(x) {
  do.call(rbind,x)
}
