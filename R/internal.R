list.if.internal <- function(x,cond,use.names=TRUE,parent=2L,envir=NULL) {
  l <- lambda(cond)
  envir <- new.env(FALSE,
    if(is.null(envir)) parent.frame(parent) else envir,.nsymbol)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  results <- Map(function(...) {
    args <- `names<-`(list(...),l$symbols)
    list2env(args,envir)
    env <- list.env(args[[1L]])
    result <- eval(l$expr,env,envir)
    if(is.logical(result)) {
      if(length(result)==1L) result
      else if(length(result>1L)) stop("Multiple values are encountered")
      else NA
    } else {
      NA
    }
  },x,seq_along(x),xnames)
  unlist(results,use.names = use.names)
}

list.findi.internal <- function(x,cond,n,parent=2L,envir=NULL) {
  l <- lambda(cond)
  envir <- new.env(FALSE,
    if(is.null(envir)) parent.frame(parent) else envir,.nsymbol)
  xnames <- names(x)
  indices <- integer()
  for(i in seq_along(x)) {
    xi <- x[[i]]
    args <- `names<-`(list(xi,i,xnames[i]),l$symbols)
    list2env(args,envir)
    env <- list.env(xi)
    result <- eval(l$expr,env,envir)
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

list.group.internal <- function(x,key,parent=2L,envir=NULL) {
  l <- lambda(key)
  envir <- new.env(FALSE,
    if(is.null(envir)) parent.frame(parent) else envir,.nsymbol)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  keys <- Map(function(...) {
    args <- `names<-`(list(...),l$symbols)
    list2env(args,envir)
    env <- list.env(args[[1L]])
    eval(l$expr,env,envir)
  },x,seq_along(x),xnames)
  unikeys <- unique(keys)
  names(unikeys) <- as.character(unikeys)
  lapply(unikeys,function(k) {
    x[vapply(keys,identical,logical(1L),y=k)]
  })
}

list.map.internal <- function(x,expr,parent=2L,envir=NULL) {
  l <- lambda(expr)
  envir <- new.env(FALSE,
    if(is.null(envir)) parent.frame(parent) else envir,.nsymbol)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  Map(function(...) {
    args <- `names<-`(list(...),l$symbols)
    list2env(args,envir)
    env <- list.env(args[[1]])
    eval(l$expr,env,envir)
  },x,seq_along(x),xnames)
}
