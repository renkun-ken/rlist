#' Join two lists by single or multiple keys
#' @param x The first list
#' @param y The second list
#' @param xkey A lambda expression that determines the key for list \code{x}
#' @param ykey A lambda expression that determines the key for list \code{y},
#'    same to \code{xkey} if \code{NULL} is taken
#' @param ... The additional parameters passed to \code{merge.data.frame}
#' @param keep.order Should the order of \code{x} be kept?
#' @param envir The environment to evaluate mapping function
#' @name list.join
#' @export
#' @examples
#' \dontrun{
#' l1 <- list(p1=list(name="Ken",age=20),
#'        p2=list(name="James",age=21),
#'        p3=list(name="Jenny",age=20))
#' l2 <- list(p1=list(name="Jenny",age=20,type="A"),
#'        p2=list(name="Ken",age=20,type="B"),
#'        p3=list(name="James",age=22,type="A"))
#' list.join(l1,l2,name)
#' list.join(l1,l2,.[c("name","age")])
#' }
list.join <- function(x,y,xkey,ykey=NULL,...,keep.order=TRUE,
  envir=parent.frame()) {
  sxkey <- substitute(xkey)
  sykey <- substitute(ykey)

  if(is.null(sxkey) && is.null(sykey))
    stop("At least one key should be specified")

  dfsxkey <- substitute(data.frame(xkey))
  if(is.null(sykey)) {
    sykey <- sxkey
    dfsykey <- substitute(data.frame(xkey))
  } else {
    dfsykey <- substitute(data.frame(ykey))
  }

  xkeys.list <- list.map.internal(x,dfsxkey,envir = envir)
  ykeys.list <- list.map.internal(y,dfsykey,envir = envir)
  xkeys.df <- list.rbind(xkeys.list)
  ykeys.df <- list.rbind(ykeys.list)
  if(is.name(sxkey)) colnames(xkeys.df) <- as.character(sxkey)
  if(is.name(sykey)) colnames(ykeys.df) <- as.character(sykey)
  if(!identical(colnames(xkeys.df),colnames(ykeys.df))) {
    stop("Inconsistent keys")
  }

  xkeys <- cbind(.xi=seq_along(xkeys.list),xkeys.df)
  ykeys <- cbind(.yi=seq_along(ykeys.list),ykeys.df)
  df <- merge.data.frame(xkeys,ykeys,by=colnames(xkeys)[-1],...)
  if(keep.order) df <- df[order(df$.xi),]
  Map(modifyList,x[df$.xi],y[df$.yi])
}
