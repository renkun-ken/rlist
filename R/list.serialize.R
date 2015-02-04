#' Serialize a list
#'
#' @param x \code{list}
#' @param file The file for output
#' @param type The type of serialization, including native serializer and
#'    json serializer, which is by default determined by file extension
#' @param ... Additional parameters passed to the serializer function
#' @seealso \code{\link{list.unserialize}}
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.serialize(x,'test.dat')
#' list.serialize(x,'test.json')
#' }
list.serialize <- function(x, file, type = tools::file_ext(file), ...) {
  fun <- paste("list.serialize", tolower(type), sep = ".")
  if (exists(fun, mode = "function")) {
    fun <- get(fun, mode = "function")
    fun(x, file, ...)
  } else {
    conn <- file(file, open = "w")
    serialize(x, conn)
    close(conn)
  }
  invisible(x)
}

list.serialize.json <- function(x, file, ...) {
  json <- jsonlite::serializeJSON(x, ...)
  writeLines(json, file)
}

#' Unserialize a file
#'
#' @param file The file as input
#' @param type The type of serialization, including native unserializer and
#'    json unserializer, which is by default determined by file extension
#' @param ... Additional parameters passed to the unserializer function
#' @seealso \code{\link{list.serialize}}
#' @export
#' @examples
#' \dontrun{
#' list.unserialize('test.dat')
#' list.unserialize('test.json')
#' }
list.unserialize <- function(file, type = tolower(tools::file_ext(file)), ...) {
  fun <- paste("list.unserialize", type, sep = ".")
  if (exists(fun, mode = "function")) {
    fun <- get(fun, mode = "function")
    fun(file, ...)
  } else {
    conn <- file(file, open = "r")
    res <- unserialize(conn)
    close(conn)
    res
  }
}

list.unserialize.json <- function(file, ...) {
  info <- file.info(file)
  txt <- readChar(file, info$size)
  jsonlite::unserializeJSON(txt, ...)
} 
