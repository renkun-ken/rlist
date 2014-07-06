list.if.function <- function(x) {
  if(is.logical(x)) {
    if(length(x) == 1L) x
    else if(length(x > 1L)) stop("Multiple values are encountered")
    else NA
  } else {
    NA
  }
}

list.if.internal <- function(.data,cond,use.names=TRUE,envir=parent.frame(2L)) {
  results <- list.map.internal(.data,cond,list.if.function,envir)
  unlist(results,use.names = use.names)
}

list.findi.internal <- function(.data,cond,n,envir=parent.frame(2L)) {
  l <- lambda(cond)
  envir <- lambda.env(envir)
  xnames <- getnames(.data)
  indices <- integer()
  for(i in seq_along(.data)) {
    xi <- .data[[i]]
    args <- setnames(list(xi,i,xnames[i]),l$symbols)
    list2env(args,envir)
    result <- eval(l$expr,list.env(xi),envir)
    if(length(indices) < n) {
      if(is.logical(result)) {
        if(length(result) == 1L && result) {
          indices <- c(indices,i)
        } else if(length(result) > 1L) {
          stop("Multiple values are encountered")
        }
      }
    } else {
      break
    }
  }
  indices
}

list.map.internal <- function(.data,expr,fun=unit,envir=parent.frame(2L)) {
  l <- lambda(expr)
  enclos <- lambda.env(envir)
  args <- c(function(.data,...) {
    list2env(list(...),enclos)
    fun(eval(l$expr,list.env(.data),enclos))
  },list(.data),list(.data),list(seq_along(.data)),list(getnames(.data)))
  names(args) <- c("f",".data",l$symbols)
  do.call(Map, args)
}

list.order.internal <- function(.data,args,envir=parent.frame(2L)) {
  envir <- new.env(parent = envir)
  list2env(list.sort.functions,envir)
  cols <- lapply(args,function(arg) {
    if(is.null(arg)) stop("NULL condition")
    unlist(list.map.internal(.data,arg,envir = envir))
  })
  do.call(order,cols)
}
