#' Load a list from file
#'
#' @param file a \code{character} vector. The file as input.
#' @param type The type of input which, by default, is determined
#'    by file extension. Currently supports RData, RDS, JSON, YAML.
#' @param ... Additional parameters passed to the loader function
#' @param guess a \code{character} vector to guess iteratively if
#' \code{type} of \code{file} is unrecognized, \code{NA} or empty
#' string.
#' @param action The post-processing action if multiple files are
#' supplied. This parameter will be ignored if only a single file
#' is supplied.
#'
#' \code{'none'} (default) to leave the resulted list as
#' a list of elements corresponding to elements in \code{file}
#' vector.
#'
#' \code{'merge'} to merge the list elements iteratively,
#' the later lists always modify the former ones through
#' \code{modifyList}.
#'
#' \code{'ungroup'} to ungroup the list elements, especially when
#' each file is a page of elements with identical structure.
#' @param progress \code{TRUE} to show a text progress bar in console
#' while loading files. By default, if \code{file} contains 5 elements,
#' then the progress bar will automatically be triggered to indicate
#' loading progress.
#' @importFrom utils txtProgressBar
#' @export
#' @examples
#' \dontrun{
#' list.load('list.rds')
#' list.load('list.rdata')
#' list.load('list.yaml')
#' list.load('list.json')
#' }
list.load <- function(file, type = tools::file_ext(file), ..., guess = c("json",
  "yaml", "rds", "rdata", "xml"), action = c("none", "merge", "ungroup"), progress = length(file) >=
  5L) {
  if (length(file) == 0L)
    return(list())
  nztype <- !is.na(type) & nzchar(type)
  fun <- paste("list.loadfile", tolower(type), sep = ".")
  fun[!nztype] <- NA_character_
  guess <- tolower(guess)
  pb <- if (progress)
    txtProgressBar(min = 0L, max = length(file), style = 3L) else NULL
  res <- if (length(file) == 1L)
    list.loadfile(file, fun, guess, ..., pb = pb, index = 1L) else {
    items <- map(list.loadfile, list(file, fun, index = seq_along(file)), list(guess = guess,
      ..., pb = pb))
    switch(match.arg(action), merge = do.call("list.merge", items), ungroup = list.ungroup(items),
      items)
  }
  if (!is.null(pb))
    close(pb)
  res
}

list.loadfile <- function(file, fun, guess, ..., pb = NULL, index = NULL) {
  res <- NULL
  if (is.na(fun)) {
    if (!missing(guess) && length(guess) > 0L) {
      exprs <- lapply(paste("list.loadfile", guess, sep = "."), function(f) call(f,
        file))
      res <- try_list(exprs, stop("Unrecognized type of file: ", file, call. = FALSE))
      if (!is.null(pb))
        pb$up(index)
    } else stop("Unrecognized type of file: ", file, call. = FALSE)
  } else if (exists(fun, mode = "function")) {
    fun <- get(fun, mode = "function")
    res <- fun(file, ...)
    if (!is.null(pb))
      pb$up(index)
  } else {
    stop("Unrecognized type of file: ", file, call. = FALSE)
  }
  res
}

list.loadfile.json <- function(file, ...) {
  callwith(jsonlite::fromJSON, list(file, simplifyDataFrame = FALSE), list(...))
}

list.loadfile.yaml <- function(file, ...) {
  yaml::yaml.load_file(file, ...)
}

list.loadfile.yml <- list.loadfile.yaml

list.loadfile.xml <- function(file, ...) {
  xmlData <- XML::xmlParse(file, ...)
  XML::xmlToList(xmlData)
}

list.loadfile.rdata <- function(file, name = "x") {
  env <- new.env(parent = parent.frame(), size = 1L)
  load(file, env)
  env[[name]]
}

list.loadfile.rds <- function(file, ...) readRDS(file, ...)
