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
#' @param x a list
#' @param query a JsonPath string
#' @param with a list of elements to be used in \code{query}
#' @importFrom V8 new_context
#' @seealso \url{https://github.com/s3u/JSONPath}
#' @export
list.query <- function(x, query, with = list()) {
  js_context <- V8::new_context(global = "window")
  if (is.list(x)) {
    js_context$assign("data", x, auto_unbox = TRUE)
  } else if (is.character(x)) {
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
