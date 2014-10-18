#' Load a list from file
#'
#' @param file The file as input
#' @param type The type of input which is by default determined
#'    by file extension
#' @param ... Additional parameters passed to the loader function
#' @name list.load
#' @export
#' @examples
#' \dontrun{
#' list.load("list.rdata")
#' list.load("list.yaml")
#' list.load("list.json")
#' }
list.load <- function(file, type = tolower(tools::file_ext(file)), ...) {
  fun <- paste("list.load", type, sep = ".")
  map(function(file, fun, ...) {
    x <- if(existsFunction(fun)) {
      fun <- get(fun, mode = "function")
      fun(file, ...)
    } else {
      list.load.rdata(file, ...)
    }
  }, file, fun, MoreArgs = list(...))
}

list.load.json <- function(file, ...) {
  jsonlite::fromJSON(file,
    simplifyDataFrame = FALSE,
    ...)
}

list.load.yaml <- function(file,...) {
  yaml::yaml.load_file(file,...)
}

list.load.yml <- list.load.yaml

list.load.rdata <- function(file,name="x") {
  env <- new.env(parent = parent.frame())
  load(file,env)
  get(name,envir = env)
}
