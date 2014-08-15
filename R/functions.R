dots <- function(...) {
  eval(substitute(alist(...)))
}

list.sort.functions <- list(desc=`-`)

setnames <- `names<-`
setclass <- `class<-`
setmembers <- `[<-`

getnames <- function(x, null = NULL)
  if(is.null(names(x))) null else names(x)

is.empty <- function(x) length(x) == 0L

set_argnames <- function(args,data=args) {
  argnames <- names(args)
  if(is.null(argnames))  argnames <- character(length(args))
  indices <- argnames=="" & vapply(args,is.name,logical(1L))
  argnames[indices] <- vapply(args[indices],as.character,character(1L))
  setnames(data,argnames)
}

#' @export
.list.env <- function(x) {
  if(is.list(x)) x
  else if(is.vector(x) && !is.null(names(x))) setclass(x,"list")
  else NULL
}
