.lambda <- list(symbols=c(".",".i",".name"))
.nsymbol <- length(.lambda$symbols)

lambda <- function(expr) {
  .lambda$expr <- expr
  if(is.call(expr) && length(expr[[1L]])==1L) {
    symbol <- as.character(expr[[1L]])
    if(symbol == "~") {
      symbols <- expr[[2L]]
      .lambda$expr <- expr[[3L]]
    } else if(symbol == "<-") {
      symbols <- expr[[3L]]
      .lambda$expr <- expr[[2L]]
    } else {
      return(.lambda)
    }
    if(is.name(symbols)) {
      .lambda$symbols[1L] <- as.character(symbols)
    } else if(is.call(symbols)) {
      symbols <- as.character(as.list(symbols)[-1])
      .lambda$symbols[seq_along(symbols)] <- symbols
    } else {
      stop("Invalid lambda expression")
    }
  }
  .lambda
}

list.env <- function(x) {
  if(is.list(x)) {
    x
  } else if(is.vector(x)) {
    as.vector(x,"list")
  } else if(is.environment(x)) {
    x
  } else {
    NULL
  }
}

list.sort.functions <- list(desc=`-`)

setnames <- `names<-`
