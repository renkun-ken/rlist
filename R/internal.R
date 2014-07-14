list.if.fun <- function(.data,.expr) {
  x <- eval(.expr,
    if(is.list(.data) || is.environment(.data)) .data
    else if(is.atomic(.data) || !is.null(names(.data))) as.vector(.data,"list")
    else NULL,
    environment())
  if(is.logical(x) && length(x) >= 1L) {
    if(length(x) == 1L) x
    else if(length(x) > 1L) stop("Multiple values are encountered")
  } else NA
}

list.if.internal <- function(.data,cond,envir=parent.frame(2L)) {
  as.logical(list.map.internal(.data,cond,list.if.fun,envir))
}

list.findi.internal <- function(.data,cond,n,envir=parent.frame(2L)) {
  if(is.null(.data) || length(.data) == 0L) return(integer(0L))
  l <- lambda(cond)
  envir <- lambda.env(envir)
  xnames <- getnames(.data,character(1))
  xnnames <- length(xnames)
  indices <- integer()
  for(i in seq_along(.data)) {
    xi <- .data[[i]]
    args <- setnames(list(xi,i,xnames[(i-1L) %% xnnames + 1L]),l$symbols)
    list2env(args,envir)
    result <- eval(l$expr,list.env(xi),envir)
    if(length(indices) < n) {
      if(is.logical(result)) {
        if(length(result) == 1L && result) indices <- c(indices,i)
        else if(length(result) > 1L) stop("Multiple values are encountered")
      }
    } else break
  }
  indices
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
