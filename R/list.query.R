list.query.internal <- function(x, filter, map, results,
  parent.i = integer(), parent.names = character()) {
  if (is.list(x)) {
    .mapply(function(x, i, name) {
      i <- c(parent.i, i)
      name <- c(parent.names, name)
      if (is.null(filter) || filter$fun(x, x, i, name)) {
        obj_name <- name
        empty_names <- !nzchar(name)
        obj_name[empty_names] <- i[empty_names]
        results[[paste0(obj_name, collapse = ".")]] <-
          if (is.null(map)) x else map$fun(x, x, i, name)
      }
      list.query.internal(x, filter, map, results = results,
        parent.i = i, parent.names = name)
      NULL
    }, list(x, seq_along(x), if (is.null(names <- names(x))) "" else names), NULL)
  }
  NULL
}

list.query.fun <- function(.data, ., .i, .name) {
  eval(.expr, .evalwith(.data), environment())
}

list.query <- function(x, filter, map, sorted = FALSE) {
  envir <- parent.frame()
  results <- new.env(parent = emptyenv())

  if (missing(filter)) {
    filter <- NULL
  } else {
    filter <- lambda(substitute(filter))
    filter$fun <- list.query.fun
    environment(filter$fun) <- args_env(.expr = filter$expr,
      .evalwith = .evalwith, parent = envir)
    formals(filter$fun) <- setnames(formals(filter$fun), c(".data", filter$symbols))
  }

  if (missing(map)) {
    map <- NULL
  } else {
    map <- lambda(substitute(map))
    map$fun <- list.query.fun
    environment(map$fun) <- args_env(.expr = map$expr,
      .evalwith = .evalwith, parent = envir)
    formals(map$fun) <- setnames(formals(map$fun), c(".data", map$symbols))
  }

  list.query.internal(x, filter, map, results)
  as.list.environment(results, sorted = sorted)
}
