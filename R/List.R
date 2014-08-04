#' List wrapper environment in which \code{data} is the \code{list} and
#' most list functions are defined for chainable operations.
#'
#' @param data \code{list}
#' @name List
#' @export
#' @details
#' Most list functions are defined in the wrapper environment.
#' In addition to these functions, \code{call(fun,...)} calls
#' external function \code{fun} with additional parameters specifies in
#' \code{...}.
#'
#' To extract the data from List \code{x}, call \code{x$data}.
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' m <- List(x)
#' m$filter(type=="B")$
#'   map(score$c1) []
#'
#' m$group(type)$
#'   map(g -> List(g)$
#'       map(score)$
#'       call(unlist)$
#'       call(mean) []) []
#' }
List <- function(data = list()) {
  envir <- environment()
  class(envir) <- c("List","environment")

  call <- function(fun,...) {
    data <- fun(data,...)
    List(data)
  }

  all <- function(...) {
    data <- list.all(data,...,envir = parent.frame())
    List(data)
  }
  any <- function(...) {
    data <- list.any(data,...,envir = parent.frame())
    List(data)
  }
  append <- function(...) {
    data <- list.append(data,...)
    List(data)
  }
  apply <- function(...) {
    data <- list.apply(data,...)
    List(data)
  }
  cases <- function(...) {
    data <- list.cases(data,...,envir = parent.frame())
    List(data)
  }
  cbind <- function() {
    data <- list.cbind(data)
    List(data)
  }
  class <- function(...) {
    data <- list.class(data,...,envir = parent.frame())
    List(data)
  }
  clean <- function(...) {
    data <- list.clean(data,...)
    List(data)
  }
  common <- function(...) {
    data <- list.common(data,...,envir = parent.frame())
    List(data)
  }
  count <- function(...) {
    data <- list.count(data,...,envir = parent.frame())
    List(data)
  }
  do <- function(...) {
    data <- list.do(data,...)
    List(data)
  }
  exclude <- function(...) {
    data <- list.exclude(data,...,envir = parent.frame())
    List(data)
  }
  extract <- function(...) {
    data <- list.extract(data,...)
    List(data)
  }
  filter <- function(...) {
    data <- list.filter(data,...,envir = parent.frame())
    List(data)
  }
  find <- function(...) {
    data <- list.find(data,...,envir = parent.frame())
    List(data)
  }
  findi <- function(...) {
    data <- list.findi(data,...,envir = parent.frame())
    List(data)
  }
  flatten <- function(...) {
    data <- list.flatten(data,...)
    List(data)
  }
  group <- function(...) {
    data <- list.group(data,...,envir = parent.frame())
    List(data)
  }
  is <- function(...) {
    data <- list.is(data,...,envir = parent.frame())
    List(data)
  }
  insert <- function(...) {
    data <- list.insert(data,...)
    List(data)
  }
  iter <- function(...) {
    data <- list.iter(data,...,envir = parent.frame())
    List(data)
  }
  join <- function(...) {
    data <- list.join(data,...,envir = parent.frame())
    List(data)
  }
  load <- function(...) {
    data <- list.load(...)
    List(data)
  }
  map <- function(...) {
    data <- list.map(data,...,envir = parent.frame())
    List(data)
  }
  mapv <- function(...) {
    data <- list.mapv(data,...,envir = parent.frame())
    List(data)
  }
  match <- function(...) {
    data <- list.match(data,...)
    List(data)
  }
  merge <- function(...) {
    data <- list.merge(data,...)
    List(data)
  }
  order <- function(...) {
    data <- list.order(data,...,.envir = parent.frame())
    List(data)
  }
  parse <- function(...) {
    data <- list.parse(...)
    List(data)
  }
  prepend <- function(...) {
    data <- list.prepend(data,...)
    List(data)
  }
  rbind <- function() {
    data <- list.rbind(data)
    List(data)
  }
  remove <- function(...) {
    data <- list.remove(data,...)
    List(data)
  }
  reverse <- function() {
    data <- list.reverse(data)
    List(data)
  }
  sample <- function(...) {
    data <- list.sample(data,...,envir = parent.frame())
    List(data)
  }
  save <- function(...) {
    data <- list.save(data,...)
    List(data)
  }
  search <- function(...) {
    data <- list.search(data,...,envir = parent.frame())
    List(data)
  }
  select <- function(...) {
    data <- list.select(data,...,.envir = parent.frame())
    List(data)
  }
  serialize <- function(...) {
    data <- list.serialize(data,...)
    List(data)
  }
  skip <- function(...) {
    data <- list.skip(data,...)
    List(data)
  }
  skipWhile <- function(...) {
    data <- list.skipWhile(data,...,envir = parent.frame())
    List(data)
  }
  sort <- function(...) {
    data <- list.sort(data,...,.envir = parent.frame())
    List(data)
  }
  stack <- function() {
    data <- list.stack(data)
    List(data)
  }
  table <- function(...) {
    data <- list.table(data,...,.envir = parent.frame())
    List(data)
  }
  take <- function(...) {
    data <- list.take(data,...)
    List(data)
  }
  takeWhile <- function(...) {
    data <- list.takeWhile(data,...,envir = parent.frame())
    List(data)
  }
  ungroup <- function(...) {
    data <- list.ungroup(data,...)
    List(data)
  }
  unserialize <- function(...) {
    data <- list.unserialize(...)
    List(data)
  }
  update <- function(...) {
    data <- list.update(data,...,.envir = parent.frame())
    List(data)
  }
  which <- function(...) {
    data <- list.which(data,...,envir = parent.frame())
    List(data)
  }
  zip <- function(...) {
    data <- list.zip(data,...)
    List(data)
  }
  subset <- function(...) {
    data <- subset(data,...,envir = parent.frame())
    List(data)
  }
  summary <- function(...) {
    data <- summary(data,...)
    List(data)
  }
  envir
}

#' @export
`[.List` <- function(x,...)
  get("data",envir = x,inherits = FALSE)

#' @export
print.List <- function(x,...) {
  cat("List environment\n")
  cat("Data:\n")
  print(x$data,...)
  invisible(x)
}

#' @export
str.List <- function(object,...) {
  cat("List environment\n")
  cat("Data:\n")
  str(object$data,...)
}

#' @export
summary.List <- function(object,...) {
  cat("List environment\n")
  cat("Data:\n")
  summary(object$data,...)
}
