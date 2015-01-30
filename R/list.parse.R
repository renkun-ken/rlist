#' Parse an object to be a list with identical structure
#'
#' @param x \code{The object}
#' @param ... Additional parameters
#' @export
#' @examples
#' x <- data.frame(a=1:3,type=c("A","C","B"))
#' list.parse(x)
#'
#' x <- matrix(rnorm(1000),ncol=5)
#' rownames(x) <- paste0("item",1:nrow(x))
#' colnames(x) <- c("a","b","c","d","e")
#' list.parse(x)
#'
#' z <- "
#' a:
#'   type: x
#'   class: A
#'   registered: yes
#' "
#' list.parse(z, type="yaml")
list.parse <- function(x, ...)
  UseMethod("list.parse")

#' @export
#' @rdname list.parse
list.parse.default <- function(x, ...) {
  as.list(x, ...)
}

#' @export
#' @rdname list.parse
list.parse.matrix <- function(x, ...) {
  apply(x, 1L, as.vector, mode = "list")
}

#' @export
#' @rdname list.parse
list.parse.data.frame <- function(x,...) {
  cols <- colnames(x)
  items <- map(function(...) setnames(list(...), cols), x)
  setnames(items, rownames(x))
}

#' @export
#' @rdname list.parse
#' @param type The type of data to parse. Currently json and yaml are supported.
list.parse.character <- function(x, type, ...) {
  if(length(x) == 0L) return(list())
  else if(length(x) == 1L) {
    if(missing(type) || length(type) == 0L || is.na(type)) {
      list.parse.default(x, ...)
    } else if(tolower(type) == "yaml") {
      yaml::yaml.load(x,...)
    } else if(tolower(type) == "json") {
      callwith(jsonlite::fromJSON,
        list(x, simplifyVector = FALSE,
          simplifyDataFrame = FALSE,
          simplifyMatrix = FALSE), list(...))
    } else if(tolower(type) == "xml") {
      XML::xmlToList(XML::xmlParseString(x, ...))
    } else {
      stop("Unsupported type of data", call. = FALSE)
    }
  } else if(length(x) > 1L) {
    map(list.parse.character, list(x, type), list(...), use.names = FALSE)
  }
}
