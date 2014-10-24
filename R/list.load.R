#' Load a list from file
#'
#' @param file The file as input
#' @param type The type of input which, by default, is determined
#'    by file extension. Currently supports RData, RDS, JSON, YAML.
#' @param ... Additional parameters passed to the loader function
#' @param action The post-processing action if multiple files are
#' supplied. This parameter will be ignored if only a single file
#' is supplied.
#'
#' \code{"none"} (default) to leave the resulted list as
#' a list of elements corresponding to elements in \code{file}
#' vector.
#'
#' \code{"merge"} to merge the list elements iteratively,
#' the later lists always modify the former ones through
#' \code{modifyList}.
#'
#' \code{"ungroup"} to ungroup the list elements, especially when
#' each file is a page of elements with identical structure.
#' @name list.load
#' @export
#' @examples
#' \dontrun{
#' list.load("list.rds")
#' list.load("list.rdata")
#' list.load("list.yaml")
#' list.load("list.json")
#' }
list.load <- function(file, type = tools::file_ext(file), ...,
  action = c("none", "merge", "ungroup")) {
  if(length(file) == 0L) return(list())
  nztype <- nzchar(type)
  nzfile <- file[!nztype]
  if(any(!nztype))
    stop("Uncertain type of sources:\n",
      paste("[", seq_along(nzfile), "] ", nzfile, sep = "", collapse = "\n"),
      "\nPlease specify the types of the sources", call. = FALSE)
  fun <- paste("list.load", tolower(type), sep = ".")
  if(length(file) == 1L) list.loadfile(file, fun, ...)
  else {
    items <- map(list.loadfile, file, fun, MoreArgs = list(...))
    switch(match.arg(action),
      merge = do.call(list.merge, items),
      ungroup = list.ungroup(items),
      items)
  }
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

list.load.rds <- function(file, ...)
  readRDS(file, ...)
