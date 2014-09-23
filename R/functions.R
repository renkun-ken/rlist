dots <- function(...) {
  eval(substitute(alist(...)))
}

setnames <- `names<-`
setclass <- `class<-`
setmembers <- `[<-`

getnames <- function(x, def = NULL)
  if(is.null(names(x))) def else names(x)

is.empty <- function(x) length(x) == 0L

set_argnames <- function(args,data = args) {
  argnames <- getnames(args, character(length(args)))
  indices <- !nzchar(argnames) & vapply(args,is.name,logical(1L))
  argnames[indices] <- as.character(args[indices])
  setnames(data,argnames)
}

#' @export
.list.env <- function(x) {
  if(is.null(names(x))) return(NULL)

  if(is.list(x)) x
  else if(is.vector(x)) setclass(x,"list")
  else NULL
}
