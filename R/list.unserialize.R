list.unserialize <- function(file,type=tolower(tools::file_ext(file)),...) {
  fun <- paste("list.unserialize",type,sep = ".")
  if(existsFunction(fun)) {
    fun <- get(fun)
    fun(file,...)
  } else {
    conn <- file(file,open="r")
    unserialize(conn)
    close(conn)
  }
}

list.unserialize.json <- function(file,...) {
  info <- file.info(file)
  txt <- readChar(file,info$size)
  jsonlite::unserializeJSON(txt,...)
}
