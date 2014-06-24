list.load <- function(file,type=tolower(tools::file_ext(file)),...) {
  fun <- paste("list.load",type,sep = ".")
  if(existsFunction(fun)) {
    fun <- get(fun)
    fun(file,...)
  } else {
    list.load.rdata(file,...)
  }
}

list.load.json <- function(file,...) {
  jsonlite::fromJSON(file,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE,...)
}

list.load.yaml <- function(file,...) {
  yaml::yaml.load_file(file,...)
}

list.load.xml <- function(file,...) {

}

list.load.rdata <- function(file,...) {
  env <- new.env(parent = parent.frame())
  load(file,env)
  env$x
}
