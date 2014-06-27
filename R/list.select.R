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
  enclos <- new.env(FALSE,parent.frame(),1)
  items <- lapply(x,function(xi) {
    env <- list.env(xi,enclos)
    lapply(args,function(arg) {
      assign(arg$symbol,xi,envir = enclos)
      eval(arg$expr,env,enclos)
    })
  })
  items
}
