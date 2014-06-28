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
  indices <- argnames=="" & vapply(args,is.name,logical(1L))
  argnames[indices] <- vapply(args[indices],as.character,character(1L))
  names(args) <- argnames
  for(i in seq_along(args)) {
    arg <- args[[i]]
    arg <- substitute(arg)
    args[[i]] <- lambda(arg)
  }
  genv <- new.env(FALSE,parent.frame(),3L)
  xnames <- if(is.null(names(x))) character(length(x)) else names(x)
  items <- Map(function(...) {
    largs <- list(...)
    env <- list.env(largs[[1L]])
    lapply(args,function(arg) {
      largs <- `names<-`(largs,arg$symbols)
      enclos <- list2env(largs,genv)
      eval(arg$expr,env,enclos)
    })
  },x,seq_along(x),xnames)
  items
}
