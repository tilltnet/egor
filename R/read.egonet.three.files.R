#' Order edge list colums with source and target at the beginning.
#' @keywords internal
order.edge.list.columns <- function(edges, source_, target) {
  cbind(edges[c(source_, target)], edges[!names(edges) %in% c(source_, target)])
}


#' Read ego-centered network data from the three files format, EgoWeb2.0 or
#' openeddi.
#' 
#' @description These functions read ego-centered network data from the three files
#' format, EgoWeb2.0 or openeddi and transform it to an egoR object. The three 
#' files format consists of an ego file, on alters file and one file containing
#' the edge data. EgoWeb2.0 and openeddi use variations of this format.
#' @template egos
#' @template alters_df
#' @param ID.vars A character vector specifying ID variable names used (egoID, 
#' alterID, source, target). 
#' @template ego_vars
#' @param edges \code{Dataframe}. A global edge list, first column is ego ID 
#' variable. 
#' egos.
#' @param alters.file \code{Character} name of the alters data file.
#' @template return_egoR
#' @examples 
#' # The data for read.egonet.threefiles() needs to be loaded with read.csv(), 
#' # for it to be converted to an egoR object.
#' egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
#' alters.file <- system.file("extdata", "alters_32.csv", package = "egor")
#' edges.file <-  system.file("extdata", "edges_32.csv", package = "egor")
#' 
#' egos <- read.csv2(egos.file)
#' alters <- read.csv2(alters.file)
#' edges <- read.csv2(edges.file)
#' 
#' tf <- read.egonet.three.files(egos = egos, alters.df = alters, edges = edges)
#' 
#' # read.egoweb() and read.openeddi() read the files directly from the disk.
#' setwd(system.file("extdata", "openeddi", package = "egor"))
#' oe <- read.openeddi()
#' 
#' setwd(system.file("extdata", "egoweb", package = "egor"))
#' ew <- read.egoweb(alter.file = "alters_32.csv", edges.file = "edges_32.csv", 
#'                   egos.file = "egos_32.csv")
#' @export
read.egonet.three.files <- function(egos, alters.df, edges, 
                                    ID.vars = c("egoID", "alterID", "Source", "Target"),
                                    ego.vars = NULL, ...) {
  # 0. Extract ID var Names
  egoID = ID.vars[[1]]
  alterID = ID.vars[[2]]
  source_ = ID.vars[[3]]
  target = ID.vars[[4]]
  
  # 3. Sort by egoID
  # Sort egos by egoID and alters by egoID and alterID.
  message("Sorting data by egoID and alterID.")
  egos <- egos[order(as.numeric(egos[[egoID]])), ]
  alters.df <- alters.df[order(as.numeric(alters.df[[egoID]]), as.numeric(alters.df[[alterID]])), ]

  # 4. (Add ego.vars, ) Reorder alters columns (alterID first!) and split to alters.df
  # - add ego.vars
  if(!is.null(ego.vars)) {
    message("Adding ego variables to alters data.")
    alters.df <- merge(alters.df, egos[c(egoID, ego.vars)],  by = egoID)
    alters.df <- alters.df[order(as.numeric(alters.df[[egoID]]), as.numeric(alters.df[[alterID]])), ]
  }

  # 7. return egor
  egor(alters.df, egos, edges, ...)
}

#' @describeIn read.egonet.three.files This function reads in data from
#' an EgoWeb 2.0 survey and transforms it to an egoR object. If no file name for
#' the egos file is provided ego data is assumed to be merged with alters data
#' and it will be extracted by \code{read.egoweb}. By default the 
#' standard ID variable names of EgoWeb are used, if you need to specify the ID
#' variable names use the ID.vars parameter. Further Information: 
#' github.com/qualintitative/egoweb
#' @param egos.file A character specifiying the filename of the ego data.
#' @param alter.file A character specifiying the filename of the alters data.
#' @param edges.file A character specifiying the filename of the edge data.
#' @importFrom utils read.csv
#' @export
read.egoweb <- function(alter.file, edges.file, egos.file = NULL, 
                        ID.vars = c("EgoID", "Alter.Number", "Alter.1.Number", 
                                    "Alter.2.Number"), ego.vars = NULL, ...) {
  # Preliminaries - Get ID var Names

  # Import CSVs
  alters.df <- read.csv(file = alter.file)
  edges <- read.csv(file = edges.file)
  if (!is.null(egos.file)) { egos <- read.csv(file = egos.file) 
   } else {
    # Extract ego data
    alterID.index <- which(names(alters.df) == ID.vars[2])
    egos <- unique(alters.df[1:(alterID.index - 1)])
    
    # Extract alter data
    egoID.index <- which(names(alters.df) == ID.vars[1])
    alters.df <- cbind(
                    alters.df[c(alterID.index, egoID.index)], 
                    alters.df[-c(alterID.index, egoID.index)])
  }
  read.egonet.three.files(egos = egos, alters.df = alters.df, edges = edges, ID.vars = ID.vars, ego.vars = ego.vars, ...)
}


#' @describeIn read.egonet.three.files This function reads in data created
#' by the openeddi survey software and transforms it to an egoR object. If no 
#' parameters are provided \code{read.openeddi} will try to find the adequate files
#' in the working directory. By default the standard ID variable names of openeddi
#' are used, if you need to specify the ID variable names use the ID.vars parameter.
#' Further Information: www.openeddi.com
#' @importFrom utils read.csv
#' @export
read.openeddi <- function(egos.file =  NULL, 
                          alters.file = NULL, 
                          edges.file = NULL, 
                          ID.vars = c("puid","nameid","nameid","targetid"), 
                          ego.vars = NULL, ...){
  if(is.null(egos.file)) {
    message("No filenames specified, looking for ego, alters and edge files in working directory.")
    files <- list.files()
    egos_index <-  grep("responses" , files)
    alters_index <-  grep("namedetails" , files)
    edges_index <-  grep("nameties" , files)
    egos.file <- files[egos_index]
    alters.file <- files[alters_index]
    edges.file <- files[edges_index]
  }
  egos <- read.csv(egos.file)
  alters.df <- read.csv(alters.file)
  edges <- read.csv(edges.file)
  
  read.egonet.three.files(egos = egos, alters.df = alters.df, edges = edges, ID.vars = ID.vars, ego.vars = ego.vars, ...)
}


