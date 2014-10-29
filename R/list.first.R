#' Find the first element that meets a condition
#' @param .data list
#' @param cond a logical lambda expression
#' @param na.rm \code{TRUE} to ignore \code{NA} values
#' @export
list.first <- function(.data, cond, na.rm = TRUE) {
  if(is.empty(.data)) return(NULL)
  if(missing(cond)) return(.data[[1L]])
  res <- list.first.internal(.data, substitute(cond), parent.frame(),
    na.stop = !na.rm)
  if(is.na(res$state)) NA else res$value
}

#' Find the last element that meets a condition
#' @param .data list
#' @param cond a logical lambda expression
#' @param na.rm \code{TRUE} to ignore \code{NA} values
#' @export
list.last <- function(.data, cond, na.rm = TRUE) {
  if(is.empty(.data)) return(NULL)
  if(missing(cond)) return(.data[[length(.data)]])
  res <- list.first.internal(rev(.data), substitute(cond), parent.frame(),
    na.stop = !na.rm)
  if(is.na(res$state)) NA else res$value
}
