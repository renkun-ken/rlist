map <- function(f, ...) {
  mapply(FUN = f, ..., SIMPLIFY = FALSE)
}

reduce <- function(f, x, init, ...) {
  y <- init
  for(xi in x) {
    y <- f(y,xi,...)
  }
  y
}

list.map.fun <- function(.data,.expr) {
  eval(.expr,.list.env(.data),environment())
}

list.map.internal <- function(.data,expr,fun = list.map.fun, envir) {
  if(is.empty(.data)) return(list())
  l <- lambda(expr)
  xnames <- getnames(.data,character(1L))
  environment(fun) <- envir
  formals(fun) <- setnames(vector("pairlist",.nfsymbol),
    c(".data",".expr",l$symbols))
  args <- list(fun,.data,list(l$expr),.data,seq_along(.data),xnames)
  do.call(map, args)
}

list.is.fun <- function(.data,.expr) {
  x <- eval(.expr,.list.env(.data),environment())
  if(is.logical(x) && length(x) == 1L) x else NA
}

list.is.internal <- function(.data,cond,envir) {
  as.logical(list.map.internal(.data,cond,list.is.fun,envir))
}

list.findi.fun <- function(.data,.expr) {
  env <- parent.frame(4L)
  env$.i <- env$.i + 1L
  x <- eval(.expr,.list.env(.data),environment())
  if(is.logical(x) && length(x) == 1L && x) {
    env$.n <- env$.n + 1L
    env$.indices <- c(env$.indices, env$.i)
    if(env$.n == env$n) stop(call. = FALSE)
  }
}

list.findi.internal <- function(.data,cond,n,envir) {
  .i <- 0L
  .n <- 0L
  .indices <- integer()
  try(list.map.internal(.data,cond,list.findi.fun,envir),silent = TRUE)
  .indices
}

list.while.fun <- function(.data,.expr) {
  env <- parent.frame(4L)
  x <- eval(.expr,.list.env(.data),environment())
  if(is.logical(x) && length(x) == 1L && x) env$.i <- env$.i + 1L
  else stop(call. = FALSE)
}

list.order.internal <- function(.data,args,envir) {
  if(is.empty(.data)) return(integer())
  if(is.empty(args)) return(order(.data))
  cols <- lapply(args,function(arg) {
    if(is.null(arg)) stop("NULL condition")
    desc <- as.character(arg)[[1L]] %in% c("desc","(")
    if(desc) arg <- arg[[2L]]
    col <- unlist(list.map.internal(.data,arg,envir = envir),use.names = FALSE)
    if(desc) col <- -xtfrm(col)
    col
  })
  names(cols) <- NULL
  do.call(order,cols)
}

list.search.fun <- function(.data, .expr, .counter, .n,
  . = .data, .i = .counter$i, .name = NA_character_) {
  q <- eval(.expr, environment(), NULL)
  vq <- !is.na(q)
  if(.counter$i < .n){
    # for logical vector, only single-valued TRUE will return value;
    # for other vectors, if it contains any non-NA values,
    # the vector will be returned
    if(is.logical(q)) {
      if(length(q) == 1L && !is.na(q) && q) {
        .counter$i <- .counter$i + length(.data)
        return(.data)
      } else {
        return(NULL)
      }
    } else if(length(q) >= 1L && any(vq)) {
      .counter$i <- .counter$i + length(which(vq))
      return(q[vq])
    }
  }
}
