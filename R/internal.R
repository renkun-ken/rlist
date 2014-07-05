list.if.internal <- function(.data,cond,use.names=TRUE,parent=2L,envir=NULL) {
  l <- lambda(cond)
  envir <- lambda.env(if(is.null(envir)) parent.frame(parent) else envir)
  xnames <- if(is.null(names(.data))) character(length(.data)) else names(.data)
  results <- Map(function(.,...) {
    args <- setnames(list(...),l$symbols)
    list2env(args,envir)
    result <- eval(l$expr,list.env(.),envir)
    if(is.logical(result)) {
      if(length(result)==1L) result
      else if(length(result>1L)) stop("Multiple values are encountered")
      else NA
    } else {
      NA
    }
  },.data,.data,seq_along(.data),xnames)
  unlist(results,use.names = use.names)
}

list.findi.internal <- function(.data,cond,n,parent=2L,envir=NULL) {
  l <- lambda(cond)
  envir <- lambda.env(if(is.null(envir)) parent.frame(parent) else envir)
  xnames <- names(.data)
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

list.map.internal <- function(.data,expr,parent=2L,envir=NULL) {
  l <- lambda(expr)
  envir <- lambda.env(if(is.null(envir)) parent.frame(parent) else envir)
  xnames <- if(is.null(names(.data))) character(length(.data)) else names(.data)
  Map(function(.,...) {
    args <- setnames(list(...),l$symbols)
    list2env(args,envir)
    eval(l$expr,list.env(.),envir)
  },.data,.data,seq_along(.data),xnames)
}

list.order.internal <- function(.data,args,parent=2L,envir=NULL) {
  envir <- lambda.env(if(is.null(envir)) parent.frame(parent) else envir)
  list2env(list.sort.functions,envir)
  cols <- lapply(args,function(arg) {
    if(is.null(arg)) stop("NULL condition")
    unlist(list.map.internal(.data,arg,envir = envir))
  })
  do.call(order,cols)
}
