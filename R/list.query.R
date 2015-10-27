list.query.internal <- function(x, filter, map, results,
  aggressive = TRUE,
  parent.i = integer(), parent.names = character()) {
  if (is.list(x)) {
    .mapply(function(x, i, name) {
      i <- c(parent.i, i)
      name <- c(parent.names, name)
      encounter <- FALSE
      if (is.null(filter) || (encounter <- filter$fun(x, x, i, name))) {
        results[[as.character(length(results) + 1L)]] <-
          if (is.null(map)) x else map$fun(x, x, i, name)
      }

      if (aggressive || !encounter) {
        list.query.internal(x, filter, map, results = results,
          aggressive = aggressive,
          parent.i = i, parent.names = name)
      }
      NULL
    }, list(x, seq_along(x), if (is.null(names <- names(x))) "" else names), NULL)
  }
  NULL
}

list.query.fun <- function(.data, ., .i, .name) {
  eval(.expr, .evalwith(.data), environment())
}

#' @export
list.query <- function(x, filter, map, ..., aggressive = TRUE) {
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

  list.query.internal(x, filter, map, results, aggressive)
  res <- as.list.environment(results)
  names(res) <- NULL
  res
}
