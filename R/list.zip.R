#' Combine multiple lists element-wisely.
#'
#' @param ... \code{list}s
#' @param use.argnames \code{logical}. Should the names of the
#'    arguments be used as the names of list items?
#' @param use.names \code{logical}. Should the names of the first
#'    argument be used as the zipped list?
#' @export
#' @seealso \code{\link{list.unzip}}
#' @examples
#' x <- list(1,2,3)
#' y <- list('x','y','z')
#' list.zip(num=x,sym=y)
list.zip <- function(..., use.argnames = TRUE, use.names = TRUE) {
  args <- list(...)
  if (use.argnames)
    args <- set_argnames(dots(...), args)
  results <- map(args_list, args)
  if (!use.names)
    names(results) <- NULL
  results
}

#' Transform a list of elements with similar structure into a list of decoupled fields
#'
#' @param .data A \code{list} of elements containing common fields
#' @param .fields \code{'intersect'} to select only common fields for
#' all \code{.data}'s elements. \code{'union'} to select any field that
#' is defined in any elements in \code{.data}.
#' @param ... The custom aggregate functions. Can be a named list of functions or
#' character vectors. If a function is specified as a list of functions, then the
#' functions will be evaluated recursively on the result of the field. Use \code{identity} to
#' avoid aggregating results. Use \code{NULL} to remove certain field.
#' @param .aggregate The default aggregate function, by default, \code{simplify2array}.
#' Can be a function, character vector or a list of functions. Use \code{identity} to avoid
#' aggregating results.
#' @param .missing When \code{.fields} is \code{'union'} and some elements do not contain
#' certain fields, then \code{NULL} will be replaced by the value of \code{.missing},
#' by default, \code{NA}. This often makes the result more friendly.
#' @importFrom stats na.omit
#' @export
#' @seealso \code{\link{list.zip}}
#' @examples
#' list.unzip(list(p1 = list(a = 1, b = 2), p2 = list(a = 2, b = 3)))
#' list.unzip(list(p1 = list(a = 1, b = 2), p2 = list(a = 2, b = 3, c = 4)))
#' list.unzip(list(p1 = list(a = 1, b = 2), p2 = list(a = 2, b = 3, c = 4)), 'union')
#' list.unzip(list(p1 = list(a = 1, b = 2), p2 = list(a = 2, b = 3, c = 4)), 'union', a = 'identity')
#' list.unzip(list(p1 = list(a = 1, b = 2), p2 = list(a = 2, b = 3, c = 4)), 'intersect', a = NULL)
#'
#' x <-
#'  list(april = list(n_days = 30,
#'    holidays = list(list('2015-04-01', 'april fools'),
#'  list('2015-04-05', 'easter')),
#'    month_info = c(number = '4', season = 'spring')),
#'      july = list(n_days = 31,
#'  holidays = list(list('2014-07-04', 'july 4th')),
#'    month_info = c(number = '7', season = 'summer')))
#' list.unzip(x, holidays = c('list.ungroup', 'unname', 'list.stack',
#'   function(df) setNames(df, c("date", "name"))))
list.unzip <- function(.data, .fields = c("intersect", "union"), ..., .aggregate = "simplify2array",
  .missing = NA) {
  data_names <- lapply(.data, names)
  aggregator <- lapply(.aggregate, match.fun)
  args <- list(...)
  if (length(args) >= 1L && (is.null(names(args)) || !all(nzchar(names(args)))))
    stop("Custom aggregate function must have a name", call. = FALSE)
  args <- lapply(args, function(f) {
    if (is.null(f))
      NULL else if (is.vector(f))
      lapply(f, match.fun) else match.fun(f)
  })
  fields <- Reduce(match.fun(match.arg(.fields)), data_names)
  names(fields) <- fields
  fields[names(args)[vapply(args, is.null, logical(1L))]] <- NA
  fields <- na.omit(fields)
  lapply(fields, function(field) {
    items <- lapply(.data, "[[", field)
    if (!is.null(.missing)) {
      missings <- vapply(items, is.null, logical(1L))
      items[missings] <- .missing
    }
    agg_fun <- if (!is.null(args[[field]]))
      args[[field]] else aggregator
    if (is.list(agg_fun)) {
      reduce(function(res, f) f(res), agg_fun, items)
    } else agg_fun(items)
  })
}
