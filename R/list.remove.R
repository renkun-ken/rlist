list.remove <- function(x,range=integer()) {
  if(is.numeric(range)) {
    x[-range]
  } else if(is.character) {
    names <- names(x)
    # exclude <- vapply(range,identical,logical(1),y=) ## not implemented yet
  }
  x[-range]
}
