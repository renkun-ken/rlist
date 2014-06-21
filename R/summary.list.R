#' Summarize a list.
#'
#' @param object The list to summarize
#' @param verbose Output list information and data briefing
#' @param show.info Output list information
#' @param show.data Output list data briefing as yaml
#' @param ... Additional parameters passed to \code{yaml::as.yaml}
#' @name summary.list
#' @return List information and data briefing in \code{yaml} format will be printed and \code{summary.default} result will be returned
#' @export
#' @examples
#' \dontrun{
#' x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
#'        p2 = list(type="B",score=list(c1=9,c2=9)),
#'        p3 = list(type="B",score=list(c1=9,c2=7)))
#' summary(x)
#' summary(x,show.info=FALSE)
#' summary(x,show.data=TRUE)
#' summary(x,show.data=2)
#' summary(x,indent=4)
#' }
summary.list <- function(object,verbose=show.info | show.data,
  show.info=TRUE,show.data=4,...) {
  if(verbose) {
    if(show.info) {
      cat(sprintf("List: %s\n",as.character(substitute(object))))
      cat(sprintf("Length: %d\n",length(object)))
    }
    if(show.data) {
      if(show.info) cat("------\n")
      if(is.logical(show.data)) {
        n <- length(object)
        cat(yaml::as.yaml(object,...))
      } else {
        n <- ceiling(show.data/2)
        object.head <- head(object,n)
        object.tail <- tail(object,min(length(object)-n,show.data-n))
        cat(yaml::as.yaml(object.head,...))
        if(length(object.head)+length(object.tail)<length(object)) {
          cat("\n...\n")
        }
        if(length(object.tail)>0)
          cat(yaml::as.yaml(object.tail,...))
      }
    }
  }
  invisible(summary.default(object))
}
