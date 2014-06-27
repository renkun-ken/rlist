lambda <- function(expr) {
  result <- list(symbols=c(".",".i",".name"),expr=expr)
  if(is.call(expr)) {
    symbol <- as.character(expr[[1]])
    if(symbol == "~") {
      symbols <- expr[[2]]
      result$expr <- expr[[3]]
    } else if(symbol == "<-") {
      symbols <- expr[[3]]
      result$expr <- expr[[2]]
    } else {
      return(result)
    }
    if(is.name(symbols)) {
      result$symbols[1] <- as.character(symbols)
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

