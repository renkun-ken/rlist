#' Query a list with JsonPath
#'
#' @details
#' JsonPath for JSON document is similar with XPath for XML documents.
#' The function creates an internal V8 Javascript engine, converts
#' the list object to Javascript DOM object, and calls
#' \url{https://github.com/s3u/JSONPath} methods to query the DOM object,
#' and finally converts the result back to list.
#'
#' The function might be less performant with large list or frequent
#' calling.
#' @param x a list, JSON string, file path or a web link to a json document.
#' @param query a JsonPath string.
#' @param with a list of elements to be used in \code{query}.
#' @param validate \code{TRUE} to validate the JSON document if \code{x}
#' is supplied one. Ignored if \code{x} is a list.
#' @param ... Additional parameters. If \code{x} is supplied a file path
#' or a web address, then these arguments will be passed to \code{readLines}
#' that reads the file content.
#' @importFrom V8 new_context
#' @importFrom jsonlite validate
#' @seealso \url{https://github.com/s3u/JSONPath}
#' @export
list.query <- function(x, query, with = list(), validate = TRUE, ...) {
  js_context <- V8::new_context(global = "window")
  if (is.empty(x)) return(x)
  if (is.list(x)) {
    js_context$assign("data", x, auto_unbox = TRUE)
  } else if (is.character(x)) {
    if (length(x) > 1L) {
      warning("Only the first value of x is taken as input")
      x <- x[1]
    }
    if (file.exists(x) || grepl("^http", x)) x <- paste(readLines(x, warn = FALSE, ...), collapse = "\n")
    if (validate) {
      val <- jsonlite::validate(x)
      if (!val) stop("Invalid JSON input:", attr(val, "err", TRUE))
    }
    js_context$eval(sprintf("var data = %s;", x))
  } else {
    stop("x must be a list or json string")
  }
  js_context$assign("options", list(sandbox = with), auto_unbox = TRUE)
  js_context$assign("jpath", query, auto_unbox = TRUE)
  js_context$source(system.file("jsonpath", "jsonpath.js", package = "rlist"))
  res <- js_context$get('JSONPath(options, data, jpath)',
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = TRUE)
  res
}
