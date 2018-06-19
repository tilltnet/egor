# Functions for the import of ego-centered-network data, that is stored in 
# seperate files (per network) and folders (alter attributes, edges).
# The code in this file is inspired by original code from Raffaele Vacca 
# (https://github.com/raffaelevacca/).

#' Read ego-centered network data exported with EgoNet software as an `egor` object
#'
#' This function imports ego-centered network data from folders with separate 
#' files for alters-level and edge data. It will run some basic checks upon
#' the completeness of the data and inform the user of potential problems. This
#' function can be used to import data exported from EgoNet (McCarty 2011).
#' @param egos.file File name of the .csv file containing the ego data.
#' @param alter.folder Folder name of the folder containing the alter data in
#' separate .csv files for each ego/ network.
#' @param edge.folder Folder name of the folder containing the edge/ tie data in
#' separate .csv files for each ego/ network.
#' @template ID.vars
#' @template return_egoR
#' @param csv.sep \code{Character} indicating the separator used in csv files.
#' @param first.col.row.names \code{Boolean} indicating if first column contains
#' row names, that are to be skipped, default is \code{FALSE}.
#' @param ... additional arguments to [egor()].
#' @keywords ego-centered import
#' @examples 
#' egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
#' alters.folder <- system.file("extdata", "alters_32", package = "egor")
#' edge.folder <-  system.file("extdata", "edges_32", package = "egor")
#' 
#' ef <- read_egonet(egos.file = egos.file, 
#'                           alter.folder = alters.folder, 
#'                           edge.folder = edge.folder, 
#'                           csv.sep = ";")
#' @importFrom utils read.csv
#' @export
read_egonet <- function(egos.file, alter.folder, edge.folder, csv.sep = ",",
                                ID.vars=list(ego="egoID", alter="alterID", source="Source", target="Target"),
                                first.col.row.names = FALSE, ...) {
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  
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
    stop("Edge and alters data do not match up!")    
  }
  
  message("Looking for egos without edges/alters.")
  # Exclude egos from egos dataframe that are missing alter and edge files.
  egos.to.exclude <- setdiff(egos[[IDv$ego]], check.alter.files)
  if(is.double(egos.to.exclude)) egos <- subset(egos, !is.element(egos[[IDv$ego]], egos.to.exclude))
    
  if (length(egos.to.exclude) > 0 ) {
    print("The following egos are excluded from the egos dataframe,")
    print("since no network data is avaiable for them:")
    print(egos.to.exclude)
  }

  
  # ...create alters df,...
  message("Creating $alters.df and $alters.list")
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
  netsize <- aggregate(alter.attr.df[[IDv$ego]], by = list(alter.attr.df[[IDv$ego]]), FUN = function(x) NROW(x))
  netsize <- merge(egos, netsize, by.x = IDv$ego, by.y = "Group.1", all.x = TRUE)$x
  
  message("Creating edge lists: $edges")
  elist.list <- list()
  j <- 1
  for (i in 1:length(edge.files)) {
    file <- edge.files[i]
    cur_egoID <- gsub("[^0-9]", "", file)
      elist.list[[i]] <- read.csv(paste(edge.folder, file, sep = "//"), sep = csv.sep, row.names = row.names)
      j <- j + 1
      names(elist.list[i]) <- cur_egoID
  }

  # Create Global edge list
  aaties <- mapply(FUN = function(x, y) data.frame(egoID = y, x), elist.list, egos[[IDv$ego]], SIMPLIFY = F)
  
  aaties.df <- do.call(rbind, aaties)
  
  # Return:
  egor(alter.attr.df, egos, aaties.df, ID.vars=IDv, ...)
  } 
