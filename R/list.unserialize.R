#' Unserialize a file
#'
#' @param file The file as input
#' @param type The type of serialization, including native unserializer and
#'    json unserializer, which is by default determined by file extension
#' @param ... Additional parameters passed to the unserializer function
#' @name list.unserialize
#' @export
#' @examples
#' \dontrun{
#' list.unserialize("test.dat")
#' list.unserialize("test.json")
#' }
list.unserialize <- function(file,type=tolower(tools::file_ext(file)),...) {
  fun <- paste("list.unserialize",type,sep = ".")
  if(exists(fun, mode = "function")) {
    fun <- get(fun, mode = "function")
    fun(file,...)
  } else {
    conn <- file(file, open="r")
    unserialize(conn)
    close(conn)
  }
}

list.unserialize.json <- function(file,...) {
  info <- file.info(file)
  txt <- readChar(file,info$size)
  jsonlite::unserializeJSON(txt,...)
}
