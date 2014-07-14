list.if.fun <- function(.data,.expr) {
  x <- eval(.expr,
    if(is.list(.data) || is.environment(.data)) .data
    else if(is.atomic(.data) || !is.null(names(.data))) as.vector(.data,"list")
    else NULL,
    environment())
  if(is.logical(x) && length(x) == 1L) x else NA
}

list.if.internal <- function(.data,cond,envir=parent.frame(2L)) {
  as.logical(list.map.internal(.data,cond,list.if.fun,envir))
}

list.findi.fun <- function(.data,.expr) {
  env <- parent.frame(4L)
  env$.i <- env$.i + 1L
  x <- eval(.expr,
    if(is.list(.data) || is.environment(.data)) .data
    else if(is.atomic(.data) || !is.null(names(.data))) as.vector(.data,"list")
    else NULL,
    environment())
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
  x <- eval(.expr,
    if(is.list(.data) || is.environment(.data)) .data
    else if(is.atomic(.data) || !is.null(names(.data))) as.vector(.data,"list")
    else NULL,
    environment())
  if(is.logical(x) && length(x) == 1L && x) env$.i <- env$.i + 1L
  else stop()
}

list.map.fun <- function(.data,.expr) {
  eval(.expr,
    if(is.list(.data) || is.environment(.data)) .data
    else if(is.atomic(.data) || !is.null(names(.data))) as.vector(.data,"list")
    else NULL,
    environment())
}

list.map.internal <- function(.data,expr,fun=list.map.fun,envir=parent.frame(2L)) {
  if(is.null(.data) || length(.data) == 0L) return(.data)
  l <- lambda(expr)
  xnames <- getnames(.data,character(1L))
  environment(fun) <- envir
  formals(fun) <- setnames(vector("list",.nfsymbol),c(".data",".expr",l$symbols))
  args <- list(fun,.data,list(l$expr),.data,seq_along(.data),xnames)
  do.call(Map, args)
}

list.order.internal <- function(.data,args,envir=parent.frame(2L)) {
  if(is.null(.data) || length(.data) == 0L) return(integer(0L))
  envir <- new.env(parent = envir)
  list2env(list.sort.functions,envir)
  cols <- lapply(args,function(arg) {
    if(is.null(arg)) stop("NULL condition")
    unlist(list.map.internal(.data,arg,envir = envir),use.names = FALSE)
  })
  do.call(order,cols)
}
