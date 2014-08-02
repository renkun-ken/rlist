#' List wrapper environment in which \code{data} is the \code{list} and
#' most list functions are defined for chainable operations.
#'
#' @param data \code{list}
#' @name List
#' @export
#' @details
#' Most list functions are defined in the wrapper environment.
#' In addition to these functions, \code{.(fun,...)} calls
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
#'   map(score$c1)$
#'   data
#'
#' m$group(type)$
#'   map(g -> List(g)$
#'       map(score)$
#'       .(unlist)$
#'       .(mean)$
#'       data)$
#'   data
#' }
List <- function(data) {
  envir = environment()
  class(envir) <- c("List","environment")
  . <- function(fun,...) {
    List(fun(data,...))
  }

  all <- function(...) {
    List(list.all(data,...))
  }
  any <- function(...) {
    List(list.any(data,...))
  }
  append <- function(...) {
    List(list.append(data,...))
  }
  apply <- function(...) {
    List(list.apply(data,...))
  }
  cases <- function(...) {
    List(list.cases(data,...))
  }
  cbind <- function() {
    List(list.cbind(data))
  }
  class <- function(...) {
    List(list.class(data,...))
  }
  clean <- function(...) {
    List(list.clean(data,...))
  }
  common <- function(...) {
    List(list.common(data,...))
  }
  count <- function(...) {
    List(list.count(data,...))
  }
  do <- function(...) {
    List(list.do(data,...))
  }
  exclude <- function(...) {
    List(list.exclude(data,...))
  }
  extract <- function(...) {
    List(list.extract(data,...))
  }
  filter <- function(...) {
    List(list.filter(data,...))
  }
  find <- function(...) {
    List(list.find(data,...))
  }
  findi <- function(...) {
    List(list.findi(data,...))
  }
  flatten <- function(...) {
    List(list.flatten(data,...))
  }
  group <- function(...) {
    List(list.group(data,...))
  }
  is <- function(...) {
    List(list.is(data,...))
  }
  insert <- function(...) {
    List(list.insert(data,...))
  }
  iter <- function(...) {
    list.iter(data,...)
    invisible(envir)
  }
  join <- function(...) {
    List(list.join(data,...))
  }
  load <- function(...) {
    List(list.load(...))
  }
  map <- function(...) {
    List(list.map(data,...))
  }
  mapv <- function(...) {
    List(list.mapv(data,...))
  }
  match <- function(...) {
    List(list.match(data,...))
  }
  merge <- function(...) {
    List(list.merge(data,...))
  }
  order <- function(...) {
    List(list.order(data,...))
  }
  parse <- function(...) {
    List(list.parse(...))
  }
  prepend <- function(...) {
    List(list.prepend(data,...))
  }
  rbind <- function() {
    List(list.rbind(data))
  }
  remove <- function(...) {
    List(list.remove(data,...))
  }
  reverse <- function() {
    List(list.reverse(data))
  }
  sample <- function(...) {
    List(list.sample(data,...))
  }
  save <- function(...) {
    List(list.save(data,...))
  }
  search <- function(...) {
    List(list.search(data,...))
  }
  select <- function(...) {
    List(list.select(data,...))
  }
  serialize <- function(...) {
    List(list.serialize(data,...))
  }
  skip <- function(...) {
    List(list.skip(data,...))
  }
  skipWhile <- function(...) {
    List(list.skipWhile(data,...))
  }
  sort <- function(...) {
    List(list.sort(data,...))
  }
  stack <- function() {
    List(list.stack(data))
  }
  table <- function(...) {
    List(list.table(data,...))
  }
  take <- function(...) {
    List(list.take(data,...))
  }
  takeWhile <- function(...) {
    List(list.takeWhile(data,...))
  }
  ungroup <- function(...) {
    List(list.ungroup(data,...))
  }
  unserialize <- function(...) {
    List(list.unserialize(...))
  }
  update <- function(...) {
    List(list.update(data,...))
  }
  which <- function(...) {
    List(list.which(data,...))
  }
  zip <- function(...) {
    List(list.zip(data,...))
  }
  subset <- function(...) {
    List(subset(data,...))
  }
  summary <- function(...) {
    List(summary(data,...))
  }
  envir
}

#' @export
print.List <- function(x,...) {
  cat("List environment\n")
  cat("Data:\n")
  print(x$data,...)
  invisible(x)
}
