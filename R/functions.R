lambda <- function(expr) {
  result <- list(symbols=c(".",".i",".name"),expr=expr)
  if(is.call(expr) && length(expr[[1L]])==1L) {
    symbol <- as.character(expr[[1L]])
    if(symbol == "~") {
      symbols <- expr[[2L]]
      result$expr <- expr[[3L]]
    } else if(symbol == "<-") {
      symbols <- expr[[3L]]
      result$expr <- expr[[2L]]
    } else {
      return(result)
    }
    if(is.name(symbols)) {
      result$symbols[1L] <- as.character(symbols)
    } else if(is.call(symbols)) {
      symbols <- as.character(as.list(symbols)[-1])
      result$symbols[seq_along(symbols)] <- symbols
    } else {
      stop("Incorrect lambda expression")
    }
  }
  result
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

