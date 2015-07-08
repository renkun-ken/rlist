#' Join two lists by single or multiple keys
#' @param x The first list
#' @param y The second list
#' @param xkey A lambda expression that determines the key for list \code{x}
#' @param ykey A lambda expression that determines the key for list \code{y},
#'    same to \code{xkey} if missing
#' @param ... The additional parameters passed to \code{merge.data.frame}
#' @param keep.order Should the order of \code{x} be kept?
#' @export
#' @importFrom utils modifyList
#' @examples
#' l1 <- list(p1=list(name='Ken',age=20),
#'        p2=list(name='James',age=21),
#'        p3=list(name='Jenny',age=20))
#' l2 <- list(p1=list(name='Jenny',age=20,type='A'),
#'        p2=list(name='Ken',age=20,type='B'),
#'        p3=list(name='James',age=22,type='A'))
#' list.join(l1, l2, name)
#' list.join(l1, l2, .[c('name','age')])
list.join <- function(x, y, xkey, ykey, ..., keep.order = TRUE) {
  if (missing(xkey) && missing(ykey))
    stop("At least one key should be specified")

  sxkey <- substitute(xkey)
  sykey <- substitute(ykey)

  dfsxkey <- substitute(data.frame(xkey))
  if (missing(sykey)) {
    sykey <- sxkey
    dfsykey <- substitute(data.frame(xkey))
  } else {
    dfsykey <- substitute(data.frame(ykey))
  }

  xkeys.list <- list.map.internal(x, dfsxkey, parent.frame())
  ykeys.list <- list.map.internal(y, dfsykey, parent.frame())
  xkeys.df <- list.rbind(xkeys.list)
  ykeys.df <- list.rbind(ykeys.list)
  if (is.name(sxkey))
    colnames(xkeys.df) <- as.character(sxkey)
  if (is.name(sykey))
    colnames(ykeys.df) <- as.character(sykey)
  if (!identical(colnames(xkeys.df), colnames(ykeys.df))) {
    stop("Inconsistent keys")
  }

  xkeys <- cbind(.xi = seq_along(xkeys.list), xkeys.df)
  ykeys <- cbind(.yi = seq_along(ykeys.list), ykeys.df)
  df <- merge.data.frame(xkeys, ykeys, by = colnames(xkeys)[-1L], ...)
  if (keep.order)
    df <- df[order(df$.xi), ]
  map(modifyList, list(x[df$.xi], y[df$.yi]))
}
