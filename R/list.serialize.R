#' Serialize a list
#'
#' @param x \code{list}
#' @param file The file for output
#' @param type The type of serialization, including native serializer and
#'    json serializer, which is by default determined by file extension
#' @param ... Additional parameters passed to the serializer function
#' @name list.serialize
#' @export
#' @examples
#' \dontrun{
#' x <- list(a=1,b=2,c=3)
#' list.serialize(x,"test.dat")
#' list.serialize(x,"test.json")
#' }
list.serialize <- function(x,file,type=tolower(tools::file_ext(file)),...) {
  fun <- paste("list.serialize",type,sep = ".")
  if(existsFunction(fun)) {
    fun <- get(fun,mode = "function")
    fun(x,file,...)
  } else {
    conn <- file(file,open="w")
    serialize(x,conn)
    close(conn)
  }
  invisible(x)
}

list.serialize.json <- function(x,file,...) {
  json <- jsonlite::serializeJSON(x,...)
  writeLines(json,file)
}
