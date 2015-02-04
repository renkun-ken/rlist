#' Save a list to a file
#'
#' @param x The list to save
#' @param file The file for output
#' @param type The type of output which, by default, is determined
#'    by file extension. Currently supports RData, RDS, JSON, YAML.
#' @param ... Additional parameters passed to the output function
#' @export
#' @return \code{x} will be returned.
#' @examples
#' \dontrun{
#' x <- lapply(1:5,function(i) data.frame(a=i,b=i^2))
#' list.save(x, 'list.rds')
#' list.save(x, 'list.rdata')
#' list.save(x, 'list.yaml')
#' list.save(x, 'list.json')
#' }
list.save <- function(x, file, type = tools::file_ext(file), ...) {
  fun <- paste("list.savefile", tolower(type), sep = ".")
  if (exists(fun, mode = "function")) {
    fun <- get(fun, mode = "function")
    fun(x, file, ...)
  } else {
    stop("Unrecognized type of file: ", file, call. = FALSE)
  }
  invisible(x)
}

list.savefile.json <- function(x, file, ...) {
  json <- jsonlite::toJSON(x, ...)
  writeLines(json, file)
}

list.savefile.yaml <- function(x, file, ...) {
  yaml <- yaml::as.yaml(x, ...)
  writeLines(yaml, file)
}

list.savefile.yml <- list.savefile.yaml

list.savefile.rdata <- function(x, file, name = "x", ...) {
  if (!is.list(x)) 
    stop("x is not a list")
  env <- new.env(parent = parent.frame(), size = 1L)
  assign(name, x, envir = env)
  save(list = name, file = file, envir = env, ...)
}

list.savefile.rds <- function(x, file, ...) saveRDS(x, file, ...) 
