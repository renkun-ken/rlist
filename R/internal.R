list.if.function <- function(x) {
  if(is.logical(x)) {
    if(length(x) == 1L) x
    else if(length(x > 1L)) stop("Multiple values are encountered")
    else NA
  } else {
    NA
  }
}

list.if.internal <- function(.data,cond,envir=parent.frame(2L)) {
  if(is.null(.data) || length(.data) == 0L) return(logical(0L))
  l <- lambda(cond)
  enclos <- lambda.env(envir)
  xnames <- list(names(.data))
  args <- c(function(.data,...) {
    list2env(list(...),enclos)
    list.if.function(eval(l$expr,list.env(.data),enclos))
  },list(.data),list(.data),list(seq_along(.data)),list(xnames))
  names(args) <- c("f",".data",l$symbols)
  as.logical(do.call(Map, args))
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

list.map.internal <- function(.data,expr,envir=parent.frame(2L)) {
  if(is.null(.data) || length(.data) == 0L) return(.data)
  l <- lambda(expr)
  enclos <- lambda.env(envir)
  xnames <- list(names(.data))
  args <- c(function(.data,...) {
    list2env(list(...),enclos)
    eval(l$expr,list.env(.data),enclos)
  },list(.data),list(.data),list(seq_along(.data)),list(xnames))
  names(args) <- c("f",".data",l$symbols)
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
