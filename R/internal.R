list.map.fun <- function(.data,.expr) {
  eval(.expr,.list.env(.data),environment())
}

list.map.internal <- function(.data,expr,fun = list.map.fun,
  envir=parent.frame(2L)) {
  if(is.null(.data) || length(.data) == 0L) return(list())
  l <- lambda(expr)
  xnames <- getnames(.data,character(1L))
  environment(fun) <- envir
  formals(fun) <- setnames(vector("list",.nfsymbol),
    c(".data",".expr",l$symbols))
  args <- list(fun,.data,list(l$expr),.data,seq_along(.data),xnames)
  do.call(Map, args)
}

list.is.fun <- function(.data,.expr) {
  x <- eval(.expr,.list.env(.data),environment())
  if(is.logical(x) && length(x) == 1L) x else NA
}

list.is.internal <- function(.data,cond,envir=parent.frame(2L)) {
  as.logical(list.map.internal(.data,cond,list.is.fun,envir))
}

list.findi.fun <- function(.data,.expr) {
  env <- parent.frame(4L)
  env$.i <- env$.i + 1L
  x <- eval(.expr,.list.env(.data),environment())
  if(is.logical(x) && length(x) == 1L && x) {
    env$.n <- env$.n + 1L
    env$.indices <- c(env$.indices, env$.i)
    if(env$.n == env$n) stop()
  }
}

list.findi.internal <- function(.data,cond,n,envir=parent.frame(2L)) {
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
  else stop()
}

list.order.internal <- function(.data,args,envir=parent.frame(2L)) {
  if(is.null(.data) || length(.data) == 0L) return(integer())
  envir <- new.env(parent = envir)
  list2env(list.sort.functions,envir)
  cols <- lapply(args,function(arg) {
    if(is.null(arg)) stop("NULL condition")
    unlist(list.map.internal(.data,arg,envir = envir),use.names = FALSE)
  })
  do.call(order,cols)
}

list.search.fun <- function(.data, .expr, .counter, .n,
  . = .data, .i = .counter$i, .name = NA_character_) {
  q <- eval(.expr, environment(), NULL)
  vq <- !is.na(q)
  if(.counter$i < .n){
    if(is.logical(q) && length(q) == 1L && !is.na(q)) {
      if(q) {
        .counter$i <- .counter$i + length(.data)
        return(.data)
      } else {
        return(NULL)
      }
    }
    if(length(q) >= 1L && any(vq)) {
      .counter$i <- .counter$i + length(which(vq))
      return(q[vq])
    }
  }
}
