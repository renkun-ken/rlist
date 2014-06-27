lambda <- function(expr) {
  if(is.call(expr) && as.character(expr[[1]])=="~") {
    list(symbol=as.character(expr[[2]]),expr=expr[[3]])
  } else if (is.call(expr) && as.character(expr[[1]])=="<-") {
    list(symbol=as.character(expr[[3]]),expr=expr[[2]])
  } else {
    list(symbol=".",expr=expr)
  }
}

list.env <- function(x,default) {
  if(is.list(x) || is.environment(x)) {
    x
  } else if(is.vector(x)) {
    as.vector(x,"list")
  } else {
    default
  }
}
