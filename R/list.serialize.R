list.serialize <- function(x,file,type=tolower(tools::file_ext(file)),...) {
  fun <- paste("list.serialize",type,sep = ".")
  if(existsFunction(fun)) {
    fun <- get(fun)
    fun(x,file,...)
  } else {
    conn <- file(file,open="w")
    serialize(x,conn)
    close(conn)
  }
}

list.serialize.json <- function(x,file,...) {
  json <- jsonlite::serializeJSON(x,...)
  writeLines(json,file)
}
