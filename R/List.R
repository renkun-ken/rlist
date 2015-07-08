createListClosure <- function(f, data) {
  f <- substitute(f)
  function(...) {
    dots <- match.call(expand.dots = FALSE)$...
    rcall <- as.call(c(f, quote(data), dots))
    data <- eval(rcall, list(data = data), parent.frame())
    List(data)
  }
}

createCallClosure <- function(data) {
  function(f, ...) {
    f <- substitute(f)
    dots <- match.call(expand.dots = FALSE)$...
    rcall <- as.call(c(f, quote(data), dots))
    data <- eval(rcall, list(data = data), parent.frame())
    List(data)
  }
}

#' Create a \code{List environment} that wraps given \code{data} and
#' most list functions are defined for chainable operations.
#'
#' @param data A \code{list} or \code{vector}
#' @export
#' @details
#' Most list functions are defined in \code{List environment}.
#' In addition to these functions, \code{call(fun,...)} calls
#' external function \code{fun} with additional parameters specifies in
#' \code{...}.
#'
#' To extract the data from List \code{x}, call \code{x$data} or simply
#' \code{x[]}.
#'
#' @examples
#' x <- list(p1 = list(type='A',score=list(c1=10,c2=8)),
#'        p2 = list(type='B',score=list(c1=9,c2=9)),
#'        p3 = list(type='B',score=list(c1=9,c2=7)))
#' m <- List(x)
#' m$filter(type=='B')$
#'   map(score$c1) []
#'
#' m$group(type)$
#'   map(g ~ List(g)$
#'       map(score)$
#'       call(unlist)$
#'       call(mean) []) []
#'
#' # Subsetting, extracting, and assigning
#'
#' p <- List(list(a=1,b=2))
#' p['a']
#' p[['a']]
#' p$a <- 2
#' p['b'] <- NULL
#' p[['a']] <- 3
List <- function(data = list()) {
  call <- createCallClosure(data)

  all <- createListClosure(list.all, data)
  any <- createListClosure(list.any, data)
  append <- createListClosure(list.append, data)
  apply <- createListClosure(list.apply, data)
  cases <- createListClosure(list.cases, data)
  cbind <- createListClosure(list.cbind, data)
  class <- createListClosure(list.class, data)
  clean <- createListClosure(list.clean, data)
  common <- createListClosure(list.common, data)
  count <- createListClosure(list.count, data)
  do <- createListClosure(list.do, data)
  exclude <- createListClosure(list.exclude, data)
  extract <- createListClosure(list.extract, data)
  filter <- createListClosure(list.filter, data)
  find <- createListClosure(list.find, data)
  findi <- createListClosure(list.findi, data)
  first <- createListClosure(list.first, data)
  flatten <- createListClosure(list.flatten, data)
  group <- createListClosure(list.group, data)
  is <- createListClosure(list.is, data)
  insert <- createListClosure(list.insert, data)
  iter <- createListClosure(list.iter, data)
  join <- createListClosure(list.join, data)
  last <- createListClosure(list.last, data)
  load <- createListClosure(list.load, data)
  map <- createListClosure(list.map, data)
  mapv <- createListClosure(list.mapv, data)
  match <- createListClosure(list.match, data)
  merge <- createListClosure(list.merge, data)
  names <- createListClosure(list.names, data)
  order <- createListClosure(list.order, data)
  parse <- createListClosure(list.parse, data)
  prepend <- createListClosure(list.prepend, data)
  rbind <- createListClosure(list.rbind, data)
  remove <- createListClosure(list.remove, data)
  reverse <- createListClosure(list.reverse, data)
  sample <- createListClosure(list.sample, data)
  save <- createListClosure(list.save, data)
  search <- createListClosure(list.search, data)
  select <- createListClosure(list.select, data)
  serialize <- createListClosure(list.serialize, data)
  skip <- createListClosure(list.skip, data)
  skipWhile <- createListClosure(list.skipWhile, data)
  sort <- createListClosure(list.sort, data)
  stack <- createListClosure(list.stack, data)
  table <- createListClosure(list.table, data)
  take <- createListClosure(list.take, data)
  takeWhile <- createListClosure(list.takeWhile, data)
  ungroup <- createListClosure(list.ungroup, data)
  unserialize <- createListClosure(list.unserialize, data)
  upzip <- createListClosure(list.unzip, data)
  update <- createListClosure(list.update, data)
  which <- createListClosure(list.which, data)
  zip <- createListClosure(list.zip, data)
  subset <- createListClosure(list.subset, data)

  envir <- environment()
  setclass(envir, c("List", "environment"))
}

#' @export
print.List <- function(x, ..., header = getOption("List.header", TRUE)) {
  if (!is.null(x$data)) {
    if (header)
      cat("$data :", class(x$data), "\n------\n")
    print(x$data, ...)
  }
}

#' @importFrom utils str
#' @export
str.List <- function(object, ..., header = getOption("List.header", TRUE)) {
  if (header)
    cat("$data : ")
  str(object$data, ...)
}

#' @export
summary.List <- function(object, ...) {
  summary(object$data, ...)
}

#' @export
`==.List` <- function(e1, e2) {
  e1$data == e2
}

#' @export
subset.List <- function(x, ...) {
  subset(x$data, ...)
}

ndots <- function(dots) {
  length(dots) >= 1L && any(nzchar(dots))
}

List_get <- function(f, data, dots, envir) {
  if (!ndots(dots))
    return(data)
  rcall <- as.call(c(f, quote(data), dots))
  data <- eval(rcall, list(data = data), envir)
  List(data)
}

List_get_function <- function(op) {
  op <- as.symbol(op)
  function(x, ...) {
    dots <- match.call(expand.dots = FALSE)$...
    List_get(op, x$data, dots, parent.frame())
  }
}

#' @export
`[.List` <- List_get_function("[")

#' @export
`[[.List` <- List_get_function("[[")


List_set <- function(f, x, dots, value, envir) {
  if (!ndots(dots))
    return(value)
  rcall <- as.call(c(f, quote(x), dots, quote(value)))
  data <- eval(rcall, list(x = x, value = value), envir)
  List(data)
}

List_set_function <- function(op) {
  op <- as.symbol(op)
  function(x, ..., value) {
    dots <- match.call(expand.dots = FALSE)$...
    List_set(op, x$data, dots, value, parent.frame())
  }
}

#' @export
`$<-.List` <- List_set_function("$<-")

#' @export
`[<-.List` <- List_set_function("[<-")

#' @export
`[[<-.List` <- List_set_function("[[<-")
