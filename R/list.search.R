#' Search a list recusively by an expression
#'
#' @param .data A \code{list} or \code{vector}
#' @param expr a lambda expression
#' @param classes a character vector of class names that restrict the search.
#' By default, the range is unrestricted (\code{ANY}).
#' @param n the maximal number of vectors to return
#' @param unlist \code{logical} Should the result be unlisted?
#' @details
#' \code{list.search} evaluates an expression (\code{expr}) recursively
#' along a list (\code{.data}).
#'
#' If the expression results in a single-valued logical vector and its
#' value is \code{TRUE}, the whole vector will be collected If it results
#' in multi-valued or non-logical vector, the non-\code{NA} values
#' resulted from the expression will be collected.
#'
#' To search whole vectors that meet certain condition, specify the
#' expression that returns a single logical value.
#'
#' To search the specific values within the vectors, use subsetting in the
#' expression, that is, \code{.[cond]} or lambda expression like
#' \code{x -> x[cond]} where \code{cond} is a logical vector used to
#' select the elements in the vector.
#' @name list.search
#' @export
#' @examples
#' # Exact search
#'
#' x <- list(p1 = list(type='A',score=c(c1=9)),
#'        p2 = list(type=c('A','B'),score=c(c1=8,c2=9)),
#'        p3 = list(type=c('B','C'),score=c(c1=9,c2=7)),
#'        p4 = list(type=c('B','C'),score=c(c1=8,c2=NA)))
#'
#' ## Search exact values
#' list.search(x, identical(., 'A'))
#' list.search(x, identical(., c('A','B')))
#' list.search(x, identical(., c(9,7)))
#' list.search(x, identical(., c(c1=9,c2=7)))
#'
#' ## Search all equal values
#' list.search(x, all(. == 9))
#' list.search(x, all(. == c(8,9)))
#' list.search(x, all(. == c(8,9), na.rm = TRUE))
#'
#' ## Search any equal values
#' list.search(x, any(. == 9))
#' list.search(x, any(. == c(8,9)))
#'
#' # Fuzzy search
#'
#' data <- list(
#'   p1 = list(name='Ken',age=24),
#'   p2 = list(name='Kent',age=26),
#'   p3 = list(name='Sam',age=24),
#'   p4 = list(name='Keynes',age=30),
#'   p5 = list(name='Kwen',age=31)
#' )
#'
#' list.search(data, grepl('^K\\w+n$', .), 'character')
#'
#' \dontrun{
#' library(stringdist)
#' list.search(data, stringdist(., 'Ken') <= 1, 'character')
#' list.search(data, stringdist(., 'Man') <= 2, 'character')
#' list.search(data, stringdist(., 'Man') > 2, 'character')
#' }
#'
#' data <- list(
#'   p1 = list(name=c('Ken', 'Ren'),age=24),
#'   p2 = list(name=c('Kent', 'Potter'),age=26),
#'   p3 = list(name=c('Sam', 'Lee'),age=24),
#'   p4 = list(name=c('Keynes', 'Bond'),age=30),
#'   p5 = list(name=c('Kwen', 'Hu'),age=31))
#'
#' list.search(data, .[grepl('e', .)], 'character')
#'
#' \dontrun{
#' list.search(data, all(stringdist(., 'Ken') <= 1), 'character')
#' list.search(data, any(stringdist(., 'Ken') > 1), 'character')
#' }
list.search <- function(.data, expr, classes = "ANY", n, unlist = FALSE) {
  vec <- rapply(.data, function(x) TRUE, classes = classes)
  if (missing(n)) n <- sum(vec)
  l <- lambda(substitute(expr))
  args <- args_env(i = 0L, n = 0L, N = n, indices = integer(n), result = vector("list", n))
  fun <- list.search.fun
  environment(fun) <- parent.frame()
  formals(fun) <- setnames(formals(fun), c(".data", ".expr", ".args", ".n", l$symbols))
  tryWithCondition(rapply(.data, fun, classes = classes, .expr = l$expr, .args = args), rlist.finished = NULL)
  result <- list.clean(args$result, recursive = FALSE)
  names(result) <- names(vec)[args$indices]
  if (unlist)
    result <- c(result, recursive = TRUE)
  result
}
