.lambda <- list(symbols=c(".",".i",".name"))
.nsymbol <- length(.lambda$symbols)

lambda <- function(expr) {
  .lambda$expr <- expr
  if(is.call(expr) && length(expr[[1L]])==1L) {
    symbol <- as.character(expr[[1L]])
    len <- length(expr)
    if(symbol == "~") {
      if(len != 3L) stop("Invalid lambda expression")
      symbols <- expr[[2L]]
      .lambda$expr <- expr[[3L]]
    } else if(symbol == "<-") {
      if(len != 3L) stop("Invalid lambda expression")
      symbols <- expr[[3L]]
      .lambda$expr <- expr[[2L]]
    } else {
      return(.lambda)
    }
    if(is.name(symbols)) {
      .lambda$symbols[1L] <- as.character(symbols)
    } else if(is.call(symbols)) {
      symbols <- as.character(as.list(symbols)[-1])
      indices <- which(symbols != "")
      .lambda$symbols[indices] <- symbols[indices]
    } else {
      stop("Invalid lambda expression")
    }
  }
  .lambda
}

lambda.env <- function(envir) {
  new.env(FALSE,envir)
}

list.env <- function(x) {
  if(is.list(x) || is.environment(x)) {
    x
  } else if(is.vector(x) && !is.null(names(x))) {
    as.vector(x,"list")
  } else if(is.matrix(x)) {
    list.parse.matrix(x)
  } else {
    NULL
  }
}

list.sort.functions <- list(desc=`-`)

setnames <- `names<-`
