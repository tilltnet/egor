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
  egos <- read.csv(egos.file, 
                   sep = csv.sep, 
                   row.names = row.names)
  
  message("Checking if alter.files and edge.files correspond")
  # Check if alter.files and edge files correspond
  alter.files <- list.files(alter.folder, full.names = TRUE)
  alter_ego_ids <- map(alter.files, basename)
  alter_ego_ids <- gsub("[^0-9]", "", alter_ego_ids)
    
  edge.files <- list.files(edge.folder, full.names = TRUE)
  edge_ego_ids <- map(edge.files, basename)
  edge_ego_ids <- gsub("[^0-9]", "", edge_ego_ids)
  
  # ...create alters df,...
  message("Creating $alters.df and $alters.list")
  
  names(alter.files) <- alter_ego_ids
  
  alter.attr.df <- 
    purrr::map_dfr(alter.files,
                 read.csv, sep = csv.sep,
                 .id = "egoID") %>%
    select(!!sym(IDv$alter), !!sym(IDv$ego), everything())
  

  message("Creating edge lists: $edges")
  names(edge.files) <- edge_ego_ids
  
  aaties.df <- 
    purrr::map_dfr(edge.files,
                   read.csv, sep = csv.sep,
                   .id = "egoID")

  # Return:
  egor(alter.attr.df, egos, aaties.df, ID.vars=IDv, ...)
  } 
