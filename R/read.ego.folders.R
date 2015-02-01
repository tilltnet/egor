# Functions for the import of ego-centric-network data, that is stored in seperate files (per network) and folders (alter attributes, dyads).
# The code in this file is inspired by original code from Raffaele Vacca (https://github.com/raffaelevacca/).

#' Import ego-centric network data from seperate folders for edgelist and alteri-attributes.
#'
#' This function imports ego-centric network data from folders with separate files for alteri-level data and dyads.
#' @param alter.folder
#' @param edge.folder
#' @keywords ego-centric network, sna
#' @export
read.egonet.folders <- function(egos, alter.folder, edge.folder, netID = "netID", ) {
  
  # Import alter attributes...
  alter.files <- list.files(alter.folder)
  
  # ...create tie df,...
  alter.attr.df <- data.frame()
  for (i in 1:length(alter.files)) {
    file <- alter.files[i]
    data <- read.csv(paste(alter.folder, file, sep = "//"))
    alter.attr.df <- rbind(alter.attr.df, data)
  }
  # ...netsize var...
  netsize <- lapply(elist.list, NROW)
  # ...and a tie list. 
  alter.attr.list <- long.df.to.list(long = egos, broad = , netsize, netID, back.to.df = F)
  
  # Import edges and edges attributes (value: list of dataframes with edges rows)
  edge.files <- list.files(edge.folder)
  
  elist.list <- list()
  for (i in 1:length(edge.files)) {
    file <- edge.files[i]
    elist.list[[i]] <- read.csv(paste(edge.folder, file, sep = "//"))
    netID <- gsub("[^0-9]", "", file)
    names(elist.list[[i]]) <- ego_ID
  }
  
  graphs <- to.network(elist = elist.list, attributes = alter.attr.list)
  
  # Return:
  list(egos.df = egos, long.df = alter.attr.df, long.list = alter.attr.list, edges = elist.list, graphs = graphs, results = data.frame(netsize))
} 