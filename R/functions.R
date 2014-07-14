.lambda <- list(symbols=c(".",".i",".name"))
.nsymbol <- length(.lambda$symbols)
.nfsymbol <- 2L + .nsymbol

lambda <- function(expr) {
  .lambda$expr <- expr
  if(is.call(expr) && length(expr[[1L]])==1L) {
    symbol <- as.character(expr[[1L]])
    if(symbol == "<-") {
      symbols <- expr[[3L]]
      .lambda$expr <- expr[[2L]]
    } else if(symbol == "~") {
      symbols <- expr[[2L]]
      .lambda$expr <- expr[[3L]]
    } else return(.lambda)

    if(is.name(symbols)) .lambda$symbols[1L] <- as.character(symbols)
    else if(is.call(symbols)) {
      symbols <- as.character(as.list(symbols)[-1L])
      indices <- which(symbols != "")
      .lambda$symbols[indices] <- symbols[indices]
    } else stop("Invalid lambda expression")
  }
  .lambda
}

lambda.env <- function(envir) new.env(FALSE,envir)

list.env <- function(x) {
  if(is.list(x) || is.environment(x)) x
  else if(is.vector(x) && !is.null(names(x))) as.vector(x,"list")
  else NULL
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

list.sort.functions <- list(desc=`-`)

setnames <- `names<-`

unname <- function(x) setnames(x,NULL)

getnames <- function(x,null=NULL) if(is.null(names(x))) null else names(x)

set_argnames <- function(args,data=args) {
  argnames <- names(args)
  if(is.null(argnames))  argnames <- character(length(args))
  indices <- argnames=="" & vapply(args,is.name,logical(1L))
  argnames[indices] <- vapply(args[indices],as.character,character(1L))
  setnames(data,argnames)
}

setmembers <- `[<-`
