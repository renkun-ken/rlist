# compatibility for data.table functions
.datatable.aware <- TRUE

#' Substitute ...
#' @param ... parameters to substitute
dots <- function(...) {
  eval(substitute(alist(...)))
}

#' Evaluate a function with a modified default values
#' @param fun either a function or a non-empty character string naming the function to be called
#' @param args a list of values to modify the default arguments of the function
#' @param dots the user-specific input (usually from ...)
#' @param keep.null \code{TRUE} to keep \code{NULL} values after argument modifications
#' @param envir the environment to evaluate the function call
#' @importFrom utils modifyList
callwith <- function(fun, args, dots = list(), keep.null = FALSE, envir = parent.frame()) {
  do.call(fun, modifyList(args, dots, keep.null = keep.null), envir = envir)
}

setnames <- `names<-`
setclass <- `class<-`
setmembers <- `[<-`

is.formula <- function(expr) {
  inherits(expr, "formula") || (is.call(expr) && expr[[1L]] == "~")
}

is.error <- function(x) {
  inherits(x, "try-error")
}

#' Test if a vector contains certain values
#' @param table the values to be matched against
#' @param x the values to be matched
contains <- function(table, x) {
  match(x, table, nomatch = 0L) > 0L
}

#' Get the names of an object
#' @details This function is used in vectorization when the names of an object
#'   is to be supplied.  \code{NULL} value will break the vectorization while
#'   setting \code{def = character(1L)} makes the names vectorizable.
#' @param x the object to extract names
#' @param def the value to return if the object has \code{NULL} names.
#' For vectorization purpose, set this to \code{character(1L)}.
getnames <- function(x, def = NULL) if (is.null(names(x))) def else names(x)

#' Check if an object is empty (has length 0)
#' @details A \code{NULL} value, zero-length vector or list have length zero,
#'   which is called empty.
#' @param x the object
is.empty <- function(x) length(x) == 0L

#' Make names for unnamed symbol arguments
#' @details The elements of an unevaluated list of arguments may or may not
#' have names as given by user. For example, \code{list.select} requires user
#' to specify the fields to select. These fields are unevaluated arguments,
#' some of which are symbols and others are calls. For the symbols, it is natural
#' to make the resulted lists to have the same name for the particular arguments.
#' @param args the unevaluated argument list
#' @param data the list to be named (\code{args} by default)
set_argnames <- function(args, data = args) {
  argnames <- getnames(args, character(length(args)))
  indices <- !nzchar(argnames) & vapply(args, is.name, logical(1L))
  argnames[indices] <- as.character(args[indices])
  setnames(data, argnames)
}

try_list <- function(exprs, finally, envir = NULL, enclos = parent.frame()) {
  for (expr in exprs) {
    result <- try(eval(expr, envir, enclos), silent = TRUE)
    if (!inherits(result, "try-error"))
      return(result)
  }
  if (missing(finally))
    stop("No valid results produced", call. = FALSE)
  eval(substitute(finally), envir, enclos)
}

#' Convert an object to evaluating environment for list elements Users should not
#' directly use this function
#' @param x the object
.evalwith <- function(x) {
  if (is.null(names(x)))
    NULL else if (is.list(x))
    x else if (is.vector(x))
    setclass(x, "list") else NULL
}

#' create an environment for args
#' @param ... objects
#' @param parent parent environment
args_env <- function(..., parent = parent.frame()) {
  list2env(list(...), parent = parent)
}

#' create a list for args
#' @param ... objects
args_list <- function(...) {
  list(...)
}
