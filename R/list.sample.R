#' Sample a list or vector
#'
#' @param .data A \code{list} or \code{vector}
#' @param size \code{integer}. The size of the sample
#' @param replace \code{logical}. Should sampling be with replacement?
#' @param weight A lambda expression to determine the weight of
#' each list member, which only takes effect if \code{prob}
#' is \code{NULL}.
#' @param prob A \code{vector} of probability weights for
#' obtaining the elements of the list being sampled.
#' @export
#' @examples
#' x <- list(a = 1, b = c(1,2,3), c = c(2,3,4))
#' list.sample(x, 2, weight = sum(.))
list.sample <- function(.data, size, replace = FALSE, weight = 1, prob = NULL) {
  if (is.null(prob)) {
    ws <- c(list.map.internal(.data, substitute(weight), parent.frame()), recursive = TRUE)
    if (any(ws < 0))
      stop("Negative weight is not allowed")
    prob <- ws/sum(ws)
  }
  sample(.data, size, replace, prob)
}
