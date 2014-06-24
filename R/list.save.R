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

list.save.rdata <- function(x,file,...) {
  if(!is.list(x)) stop("x is not a list")
  save(x,file = file,...)
}
