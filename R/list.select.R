#' Select by name or expression for each member of a list
#' @param x The list
#' @param ... The members to select
#' @name list.select
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' list.select(x,type)
#' list.select(x,tp=type)
#' list.select(x,type,score)
#' list.select(x,type,score.range=range(unlist(score)))
#' }
list.select <- function(x,...) {
  args <- as.list(match.call(expand.dots = FALSE))$`...`
  argnames <- names(args)
  if(is.null(argnames)) {
    argnames <- character(length(args))
  }
  indices <- argnames=="" & vapply(args,is.name,logical(1))
  argnames[indices] <- vapply(args[indices],as.character,character(1))
  names(args) <- argnames
  for(i in seq_along(args)) {
    arg <- args[[i]]
    arg <- substitute(arg)
    args[[i]] <- lambda(arg)
  }
  enclos <- new.env(parent = parent.frame(),size = 3)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  items <- Map(function(xi,i,name) {
    env <- list.env(xi,enclos)
    enclos$.i <- i
    enclos$.name <- name
    lapply(args,function(arg) {
      enclos[[arg$symbol]] <- xi
      eval(arg$expr,env,enclos)
    })
  },x,seq_along(x),xnames)
  items
}
