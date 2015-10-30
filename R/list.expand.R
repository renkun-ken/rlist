#' Create a list from all combinations of factors
#'
#' Create a list from all combinations of the supplied
#' vectors or lists, extending the functionality of
#' \link{expand.grid} from data frame to list.
#'
#' @param ... vectors or lists
#'
#' @return
#' A list of all combinations of the supplied vectors or
#' lists.
#' @export
#' @examples
#' list.expand(x=1:10, y=c("a","b","c"))
#' list.expand(x=list(c(1,2), c(2,3)), y = c("a","b","c"))
#' list.expand(
#'   a=list(list(x=1,y="a"), list(x=2, y="b")),
#'   b=list(c("x","y"), c("y","z","w")))
list.expand <- function(...) {
  args <- list(...)
  expand_args <- lapply(args, seq_along)
  expand_df <- do.call(expand.grid, expand_args)
  .mapply(function(...) {
    mapply(`[[`, args, list(...), USE.NAMES = TRUE, SIMPLIFY = FALSE)
  }, expand_df, NULL)
}
