#' Extract an element from a list or vector
#' @export
#' @examples
#' x <- list(a=1, b=2, c=3)
#' list.extract(x, 1)
#' list.extract(x, 'a')
list.extract <- `[[`
