require_shiny <- function(cap=NULL){
  if(is.null(cap)) cap <- "This capability"
  if(!requireNamespace("shiny", quietly=TRUE)){
    stop(cap, " requires the R package ", sQuote("shiny"), " to be installed.", call.=FALSE)
  }
}

require_igraph <- function(cap=NULL){
  if(is.null(cap)) cap <- "This capability"
  if(!requireNamespace("igraph", quietly=TRUE)){
    stop(cap, " requires the R package ", sQuote("igraph"), " to be installed.", call.=FALSE)
  }
}
