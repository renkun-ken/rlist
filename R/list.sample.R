#' Sample a list
#'
#' @param x The list
#' @param size The size the sample to take
#' @param replace Should sampling be with replacement?
#' @param weight An expression to determine the weight of each list member,
#'    which does only takes effectt if \code{prob} is \code{NULL}.
#' @param prob A vector of probability weights for obtaining the elements of the
#'    list being sampled.
#' @name list.sample
#' @export
#' @examples
#' \dontrun{
#' x <- lapply(1:3,function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3,function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.rbind(x)
#' list.rbind(df)
#' }
list.sample <- function(x,size,replace=FALSE,weight=1,prob=NULL) {
  if(is.null(prob)) {
    weight <- substitute(weight)
    ws <- unlist(list.map.internal(x,weight),use.names = FALSE)
    if(any(ws<0)) stop("Negative weight is not allowed")
    prob <- ws / sum(ws)
  }
  sample(x,size,replace,prob)
}
