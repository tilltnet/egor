#' Import ego-centric network data from seperate folders for edgelist and alteri-attributes.
#'
#' This function imports ego-centric network data from folders with separate files for alteri-level data and dyad-level data.
#' @param alter.folder
#' @param edge.folder
#' @keywords ego-centric network, sna
#' @export


read.egonet.folders <- function(alter.folder, edge.folder) {

  alter.files <- list.files(alter.folder)
  
  alter.attr.df <- data.frame()
  for (i in 1:length(alter.files)) {
    file <- alter.files[i]
    data <- read.csv(paste(alter.folder, file, sep="//"))
    alter.attr.df <- rbind(alter.attr.df, data)
  }
  
  edge.files <- list.files(edge.folder)
  
  elist.list <- list()
  for (i in 1:length(edge.files)) {
    file <- edge.files[i]
    elist.list[[i]] <- read.csv(paste(edge.folder, file, sep="//"))
    ego_ID <- gsub("[^0-9]", "", file)
    names(elist.list[[i]]) <- ego_ID
  }
  # Return:
  list(alter.attr.df, elist.list)
}
