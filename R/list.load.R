#' Load a list from file
#'
#' @param file The file as input
#' @param type The type of input which, by default, is determined
#'    by file extension. Currently supports RData, RDS, JSON, YAML.
#' @param ... Additional parameters passed to the loader function
#' @name list.load
#' @export
#' @examples
#' \dontrun{
#' list.load("list.rds")
#' list.load("list.rdata")
#' list.load("list.yaml")
#' list.load("list.json")
#' }
list.load <- function(file, type = tools::file_ext(file), ...) {
  if(length(file) == 0L) return(list())
  type <- tolower(type)
  nztype <- nzchar(type)
  nzfile <- file[!nztype]
  if(any(!nztype))
    stop("Uncertain type of sources:\n",
      paste("[", seq_along(nzfile), "] ", nzfile, sep = "", collapse = "\n"),
      "\nPlease specify the types of the sources", call. = FALSE)
  fun <- paste("list.load", type, sep = ".")
  if(length(file) == 1L) list.loadfile(file, fun, ...)
  else map(list.loadfile, file, fun, MoreArgs = list(...))
}

list.loadfile <- function(file, fun, ...) {
  envir <- parent.frame()
  if(exists(fun, envir = envir, mode = "function")) {
    fun <- get(fun, envir = envir, mode = "function")
    fun(file, ...)
  } else {
    stop("Unrecognized file type", call. = FALSE)
  }
}

list.load.json <- function(file, ...) {
  callwith(jsonlite::fromJSON,
    list(file, simplifyDataFrame = FALSE), list(...))
}

list.load.yaml <- yaml::yaml.load_file

list.load.yml <- list.load.yaml

list.load.rdata <- function(file, name = "x") {
  env <- new.env(parent = parent.frame(), size = 1L)
  load(file, env)
  env[[name]]
}

list.load.rds <- readRDS
