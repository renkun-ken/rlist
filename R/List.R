# compatibility for data.table functions
.datatable.aware <- TRUE

#' Create a \code{List environment} that wraps given \code{data} and
#' most list functions are defined for chainable operations.
#'
#' @param data \code{list}
#' @name List
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
  call <- function(f,...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(f,data),dots))
    data <- eval(rcall, envir = parent.frame())
    List(data)
  }

  all <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.all,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  any <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.any,data),dots))
    data <- eval(rcall,parent.frame())
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
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.cases,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  cbind <- function() {
    data <- list.cbind(data)
    List(data)
  }
  class <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.class,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  clean <- function(...) {
    data <- list.clean(data,...)
    List(data)
  }
  common <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.common,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  count <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.count,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  do <- function(...) {
    data <- list.do(data,...)
    List(data)
  }
  exclude <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.exclude,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  extract <- function(...) {
    data <- list.extract(data,...)
    List(data)
  }
  filter <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.filter,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  find <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.find,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  findi <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.findi,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  flatten <- function(...) {
    data <- list.flatten(data,...)
    List(data)
  }
  group <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.group,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  is <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.is,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  insert <- function(...) {
    data <- list.insert(data,...)
    List(data)
  }
  iter <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.iter,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  join <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.join,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  load <- function(...) {
    data <- list.load(...)
    List(data)
  }
  map <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.map,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  mapv <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.mapv,data),dots))
    data <- eval(rcall,parent.frame())
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
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.order,data),dots))
    data <- eval(rcall,parent.frame())
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
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.sample,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  save <- function(...) {
    data <- list.save(data,...)
    List(data)
  }
  search <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.search,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  select <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.select,data),dots))
    data <- eval(rcall,parent.frame())
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
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.skipWhile,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  sort <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.sort,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  stack <- function() {
    data <- list.stack(data)
    List(data)
  }
  table <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.table,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  take <- function(...) {
    data <- list.take(data,...)
    List(data)
  }
  takeWhile <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.takeWhile,data),dots))
    data <- eval(rcall,parent.frame())
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
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.update,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  which <- function(...) {
    dots <- match.call(expand.dots = FALSE)$`...`
    rcall <- as.call(c(list(list.which,data),dots))
    data <- eval(rcall,parent.frame())
    List(data)
  }
  zip <- function(...) {
    data <- list.zip(data,...)
    List(data)
  }
  subset <- function(...) {
    data <- list.subset(data,...)
    List(data)
  }
  summary <- function(...) {
    data <- summary(data,...)
    List(data)
  }
  envir <- environment()
  setclass(envir, c("List","environment"))
}

#' @export
`[.List` <- function(x,...)
  get("data",envir = x,inherits = FALSE)

#' @export
print.List <- function(x,...,header = getOption("List.header", TRUE)) {
  if(!is.null(x$data)) {
    if(header) cat("$data :",class(x$data),"\n------\n")
    print(x$data,...)
  }
}

#' @export
str.List <- function(object,...,header = getOption("List.header", TRUE)) {
  if(header) cat("$data : ")
  str(object$data,...)
}

#' @export
summary.List <- function(object,...) {
  summary(object$data,...)
}

#' @export
`==.List` <- function(e1,e2) {
  e1$data == e2
}

#' @export
subset.List <- function(x,...) {
  subset(x$data,...)
}
