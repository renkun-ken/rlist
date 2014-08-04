#' Sample a list
#'
#' @param .data \code{list}
#' @param size \code{integer}. The size of the sample
#' @param replace \code{logical}. Should sampling be with replacement?
#' @param weight A lambda expression to determine the weight of
#' each list member, which does only takes effect if \code{prob}
#' is \code{NULL}.
#' @param prob A \code{vector} of probability weights for
#' obtaining the elements of the list being sampled.
#' @param envir The environment to evaluate mapping function
#' @name list.sample
#' @export
#' @examples
#' \dontrun{
#' x <- lapply(1:3,function(i) { c(a=i,b=i^2)})
#' df <- lapply(1:3,function(i) { data.frame(a=i,b=i^2,c=letters[i])})
#' list.rbind(x)
#' list.rbind(df)
#' }
list.sample <- function(.data,size,replace=FALSE,weight=1,prob=NULL,
  envir = parent.frame()) {
  if(is.null(prob)) {
    ws <- unlist(list.map.internal(.data,substitute(weight),envir = envir),
      use.names = FALSE)
    if(any(ws<0)) stop("Negative weight is not allowed")
    prob <- ws / sum(ws)
  }
  sample(.data,size,replace,prob)
}
