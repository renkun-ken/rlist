#' Parse an object to be a list with identical structure
#'
#' @param x \code{The object}
#' @param ... Additional parameters
#' @name list.parse
#' @export
#' @examples
#' \dontrun{
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
#' list.parse(z,type="yaml")
#' }
list.parse <- function(x,...)
  UseMethod("list.parse")

#' @export
#' @rdname list.parse
list.parse.default <- function(x,...) {
  as.list(x,...)
}

#' @export
#' @rdname list.parse
list.parse.matrix <- function(x,...) {
  apply(x,1L,as.vector,mode="list")
}

#' @export
#' @rdname list.parse
list.parse.data.frame <- function(x,...) {
  cols <- colnames(x)
  items <- do.call(Map,c(function(...) {
   setnames(list(...),cols)
  },x))
  setnames(items,rownames(x))
}

#' @export
#' @rdname list.parse
#' @param type The type of data to parse.
#'    Currently json and yaml are supported.
#'    In default, \code{NULL} value will parse the character with
#'    \code{list.parse.default}. Ignored when the length of \code{x}
#'    is greater than 1.
list.parse.character <- function(x,...,type=NULL) {
  if(length(x) > 1L) return(list.parse.default(x,...))
  if(is.null(type)) {
    list.parse.default(x,...)
  } else if(tolower(type)=="yaml") {
    yaml::yaml.load(x,...)
  } else if(tolower(type)=="json") {
    jsonlite::fromJSON(x,
      simplifyVector = FALSE,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE,...)
  } else {
    stop("Invalid type of data")
  }
}

