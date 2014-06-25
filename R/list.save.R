#' Save a list to a file
#'
#' @param x The list to save
#' @param file The file for output
#' @param type The type of output which is by default determined
#'    by file extension
#' @param ... Additional parameters passed to the output function
#' @name list.save
#' @export
#' @examples
#' \dontrun{
#' x <- lapply(1:5,function(i) data.frame(a=i,b=i^2))
#' list.save(x,"list.rdata")
#' list.save(x,"list.yaml")
#' list.save(x,"list.json")
#' }
list.save <- function(x,file,type=tolower(tools::file_ext(file)),...) {
  fun <- paste("list.save",type,sep = ".")
  if(existsFunction(fun)) {
    fun <- get(fun)
    fun(x,file,...)
  } else {
    list.save.rdata(x,file,...)
  }
}

list.save.json <- function(x,file,...) {
  json <- jsonlite::toJSON(x,...)
  writeLines(json,file)
}

list.save.yaml <- function(x,file,...) {
  yaml <- yaml::as.yaml(x,...)
  writeLines(yaml,file)
}

list.save.yml <- list.save.yaml

list.save.xml <- function(x,file,...) {
  root <- XML::newXMLNode("root")
  root <- list.to.xml(root,x)
  XML::saveXML(root,file,...)
}

list.to.xml <- function(node,sublist) {
  names <- names(sublist)
  for(i in seq_along(sublist)){
    item <- sublist[[i]]
    child <- XML::newXMLNode(names[i], parent=node)
    if (is.list(item)){
      list.to.xml(child, item)
    }
    else{
      XML::xmlValue(child) <- item
    }
  }
  node
}

list.save.rdata <- function(x,file,name="x",...) {
  if(!is.list(x)) stop("x is not a list")
  env <- new.env(parent = parent.frame(), size=1)
  assign(name,x,envir = env)
  save(list=name,file = file,...,envir = env)
}
