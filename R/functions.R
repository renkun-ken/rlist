lambda <- function(expr) {
  if(is.call(expr) && as.character(expr[[1]])=="~") {
    list(symbol=as.character(expr[[2]]),expr=expr[[3]])
  } else if (is.call(expr) && as.character(expr[[1]])=="<-") {
    list(symbol=as.character(expr[[3]]),expr=expr[[2]])
  } else {
    list(symbol=".",expr=expr)
  }
}
