# Functions for the import of ego-centric-network data, that is stored in 
# seperate files (per network) and folders (alter attributes, edges).
# The code in this file is inspired by original code from Raffaele Vacca 
# (https://github.com/raffaelevacca/).

#' Import ego-centric network data from separate folders for edgelist and 
#' alteri-attributes.
#'
#' This function imports ego-centric network data from folders with separate 
#' files for alteri-level and edge data. It will run some basic checks upon
#' the completness of the data and inform the user of potential problems.
#' @param egos.file File name of the .csv file containg the ego data.
#' @param alter.folder Folder name of the folder containing the alter data in
#' separate .csv files for each ego/ network.
#' @param edge.folder Folder name of the folder containing the edge/ tie data in
#' separate .csv files for each ego/ network.
#' @template egoID
#' @template return_egoR
#' @keywords ego-centric network, sna
#' @export
read.egonet.folders <- function(egos.file, alter.folder, edge.folder, csv.sep = ",",
                                egoID = "egoID", first.col.row.names = FALSE) {
  
  # if first.col.row.names is TRUE:
  if(first.col.row.names) {row.names <- 1} else {row.names <- NULL}
  
  # Import ego data
  message("Reading ego data.")
  egos <- read.csv(egos.file, sep = csv.sep, row.names = row.names)
  
  message("Checking if alter.files and edge.files correspond")
  # Check if alter.files and edge files correspond
  alter.files <- list.files(alter.folder)
  edge.files <- list.files(edge.folder)
  check.alter.files <- gsub("[^0-9]", "", alter.files)
  check.edge.files <- gsub("[^0-9]", "", edge.files)
  check <- check.alter.files == check.edge.files
  
  if (!all.equal(check.alter.files, check.edge.files)) {
    print(data.frame(check, alter.files, edge.files))
    stop("Edge and alteri data do not match up!")    
  }
  
  message("Looking for egos without edges/alteri.")
  # Exclude egos from egos dataframe that are missing alter and edge files.
  egos.to.exclude <- setdiff(egos[[egoID]], check.alter.files)
  if(is.double(egos.to.exclude)) egos <- subset(egos, !is.element(egos[[egoID]], egos.to.exclude))
    
  if (length(egos.to.exclude) > 0 ) {
    print("The following egos are excluded from the egos dataframe,")
    print("since no network data is avaiable for them:")
    print(egos.to.exclude)
  }

  
  # ...create tie df,...
  message("Creating $alteri.df and $alteri.list")
  alter.attr.df <- data.frame()
  alter.attr.list <- list()
  for (i in 1:length(alter.files)) {
    #print(i)
    file <- alter.files[i]
    data <- read.csv(paste(alter.folder, file, sep = "//"), sep = csv.sep, row.names = row.names)
    alter.attr.df <- rbind(alter.attr.df, data)
    alter.attr.list[[i]] <- data
  }
  # ...netsize var...
  message("Calculating netsize values.")
  netsize <- aggregate(alter.attr.df[[egoID]], by = list(alter.attr.df[[egoID]]), FUN = function(x) NROW(x))
  netsize <- merge(egos, netsize, by.x = egoID, by.y = "Group.1", all.x = T)$x
  
  message("Creating edge lists: $edges")
  elist.list <- list()
  j <- 1
  for (i in 1:length(edge.files)) {
    file <- edge.files[i]
    cur_egoID <- gsub("[^0-9]", "", file)
    #if (is.element(cur_egoID, egos[[egoID]])) {
      elist.list[[i]] <- read.csv(paste(edge.folder, file, sep = "//"), sep = csv.sep, row.names = row.names)
      j <- j + 1
      names(elist.list[i]) <- cur_egoID
  }

  graphs <- to.network(elist = elist.list, alteri.list = alter.attr.list)
  
  # Return:
  list(egos.df = egos, long.df = alter.attr.df, long.list = alter.attr.list, edges = elist.list, graphs = graphs, results = data.frame(netsize))
} 