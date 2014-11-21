.expr <- NULL

map <- function(f, ...) {
  mapply(FUN = f, ..., SIMPLIFY = FALSE)
}

reduce <- function(f, x, init, ...) {
  y <- init
  for(xi in x) y <- f(y, xi, ...)
  y
}

list.map.fun <- function(.data, ., .i, .name) {
  eval(.expr, .evalwith(.data), environment())
}

list.map.internal <- function(.data, expr, envir,
  fun = list.map.fun, args = NULL) {
  if(is.empty(.data)) return(list())
  l <- lambda(expr)
  xnames <- getnames(.data, character(1L))
  environment(fun) <- args_env(.expr = l$expr,
    .args = args, .evalwith = .evalwith, parent = envir)
  formals(fun) <- setnames(formals(fun), c(".data", l$symbols))
  args <- list(fun, .data, .data, seq_along(.data), xnames)
  do.call("map", args)
}

list.is.fun <- function(.data, ., .i, .name) {
  x <- eval(.expr, .evalwith(.data), environment())
  if(is.logical(x) && length(x) == 1L) x else NA
}

list.is.internal <- function(.data, cond, envir) {
  as.logical(list.map.internal(.data, cond, envir, list.is.fun))
}

list.findi.fun <- function(.data, ., .i, .name) {
  .args$i <- .args$i + 1L
  x <- eval(.expr, .evalwith(.data), environment())
  if(identical(x, TRUE)) {
    .args$n <- .args$n + 1L
    .args$indices[[.args$n]] <- .args$i
    if(.args$n == .args$N) stop("finished", call. = FALSE)
  } else if(identical(x, FALSE)) {
    # do nothing
  } else if(.args$na.stop) {
    stop("stopped", call. = FALSE)
  }
}

list.findi.internal <- function(.data, cond, envir, n, na.stop = FALSE) {
  n <- min(n, length(.data))
  args <- args_env(i = 0L, n = 0L, N = n, na.stop = na.stop, indices = integer(n))
  result <- try(list.map.internal(.data, cond, envir,
    list.findi.fun, args), silent = TRUE)
  if(is.error(result)) {
    switch(attr(result, "condition")$message,
      finished = args$indices[0L:args$n],
      stopped = NULL,
      integer())
  } else {
    return(integer())
  }
}

list.first.fun <- function(.data, ., .i, .name) {
  x <- eval(.expr, .evalwith(.data), environment())
  if(identical(x, TRUE)) {
    .args$res <- list(state = TRUE, value = .data)
    stop(call. = FALSE)
  } else if(identical(x, FALSE)) {
    # do nothing
  } else if(.args$na.stop) {
    .args$res <- list(state = NA, value = .data)
    stop(call. = FALSE)
  }
}

list.first.internal <- function(.data, cond, envir, na.stop) {
  args <- args_env(res = list(state = FALSE))
  try(list.map.internal(.data, cond, envir, list.first.fun, args),
    silent = TRUE)
  args$res
}

list.while.fun <- function(.data, ., .i, .name) {
  x <- eval(.expr, .evalwith(.data), environment())
  if(identical(x, TRUE)) .args$i <- .args$i + 1L
  else stop(call. = FALSE)
}

list.order.internal <- function(.data, args, envir) {
  if(is.empty(.data)) return(integer())
  if(is.empty(args)) return(order(.data))
  cols <- lapply(args, function(arg) {
    if(is.null(arg)) stop("NULL condition", call. = FALSE)
    desc <- class(arg) == "("
    if(is.call(arg) && arg[[1L]] == "desc") {
      warning("desc() has been deprecated. Please use () to indicate descending order. Example: list.sort(data, (count))", call. = FALSE)
      desc <- TRUE
    }
    if(desc) arg <- arg[[2L]]
    col <- unlist(list.map.internal(.data, arg, envir),
      recursive = FALSE, use.names = FALSE)
    if(desc) col <- -xtfrm(col)
    col
  })
  names(cols) <- NULL
  do.call("order",cols)
}

list.search.fun <- function(.data, .expr, .counter, .n,
  . = .data, .i = .counter$i, .name = NULL) {
  q <- eval(.expr, environment())
  vq <- !is.na(q)
  if(.counter$i < .n){
    # for logical vector, only single-valued TRUE will return value;
    # for other vectors, if it contains any non-NA values,
    # the vector will be returned
    if(is.logical(q)) {
      if(identical(q, TRUE)) {
        .counter$i <- .counter$i + 1L
        return(.data)
      } else {
        return(NULL)
      }
    } else if(length(q) >= 1L && any(vq)) {
      .counter$i <- .counter$i + 1L
      return(q[vq])
    }
  }
}

list.group.internal <- function(.data, keys, envir, proc = NULL,
  compare = "identical", sorted = TRUE) {
  if(is.empty(keys)) return(.data)
  values <- list.map.internal(.data, keys[[1L]], envir)
  uvalues <- if(!missing(proc) && !is.null(proc))
    match.fun(proc)(values) else values
  uniques <- unique.default(uvalues)
  names(uniques) <- uniques
  if(sorted && all(vapply(uniques, length, integer(1L)) == 1L))
    uniques <- sort(unlist(uniques))
  lapply(uniques, function(key, ...) {
    selector <- vapply(values, compare, logical(1L), key, USE.NAMES = FALSE)
    data <- .data[selector]
    list.group.internal(data, keys[-1L], ...)
  }, proc, compare, sorted, envir)
}
