#' Order edge list columns with source and target at the beginning.
#' @keywords internal
order.edge.list.columns <- function(edges, source_, target) {
  cbind(edges[c(source_, target)], edges[!names(edges) %in% c(source_, target)])
}


#' Read/ import ego-centered network data from the three files format, EgoWeb2.0 or
#' openeddi.
#' 
#' @description These functions read ego-centered network data from the three files
#' format, EgoWeb2.0 or openeddi and transform it to an egoR object. The three 
#' files format consists of an ego file, on alters file and one file containing
#' the edge data. EgoWeb2.0 and openeddi use variations of this format.
#' @template egos
#' @template alters_df
#' @template ID.vars
#' @template ego_vars
#' @param edges \code{Dataframe}. A global edge list, first column is ego ID 
#' variable. 
#' egos.
#' @param alters.file \code{Character} name of the alters data file.
#' @param ... additional arguments to [egor()].
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
#' tf <- threefiles_to_egor(egos = egos, alters.df = alters, edges = edges)
#' 
#' # read_egoweb() and read_openeddi() read the files directly from the disk.
#' \donttest{
#' # Fetch current working directory
#' wd <- getwd()
#' 
#' setwd(system.file("extdata", "openeddi", package = "egor"))
#' oe <- read_openeddi()
#' 
#' setwd(system.file("extdata", "egoweb", package = "egor"))
#' ew <- read_egoweb(alter.file = "alters_32.csv", edges.file = "edges_32.csv", 
#'                   egos.file = "egos_32.csv")
#'                   
#' # Restore working directory                   
#' setwd(wd)
#' }
#' @export
threefiles_to_egor <- function(egos, alters.df, edges, 
                                    ID.vars = list(ego="egoID", 
                                                   alter="alterID", 
                                                   source="Source", 
                                                   target="Target"),
                                    ego.vars = NULL, ...) {
  # 0. Extract ID var Names
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  
  # 3. Sort by IDv$ego
  # Sort egos by IDv$ego and alters by IDv$ego and IDv$alter.
  egos <- egos[order(as.numeric(egos[[IDv$ego]])), ]
  alters.df <- alters.df[order(as.numeric(alters.df[[IDv$ego]]), as.numeric(alters.df[[IDv$alter]])), ]

  # 4. (Add ego.vars, ) Reorder alters columns (IDv$alter first!) and split to alters.df
  # - add ego.vars
  if(!is.null(ego.vars)) {
    message("Adding ego variables to alters data.")
    alters.df <- merge(alters.df, egos[c(IDv$ego, ego.vars)],  by = IDv$ego)
    alters.df <- alters.df[order(as.numeric(alters.df[[IDv$ego]]), as.numeric(alters.df[[IDv$alter]])), ]
  }

  # 7. return egor
  egor(alters.df, egos, edges, ID.vars = IDv, ...)
}

#' @describeIn threefiles_to_egor This function reads in data from
#' an EgoWeb 2.0 survey and transforms it to an egoR object. If no file name for
#' the egos file is provided ego data is assumed to be merged with alters data
#' and it will be extracted by \code{read_egoweb}. By default the 
#' standard ID variable names of EgoWeb are used, if you need to specify the ID
#' variable names use the ID.vars parameter. Further Information: 
#' github.com/qualintitative/egoweb
#' @param egos.file A character specifiying the filename of the ego data.
#' @param alter.file A character specifiying the filename of the alters data.
#' @param edges.file A character specifiying the filename of the edge data.
#' @importFrom utils read.csv
#' @export
read_egoweb <- function(alter.file, edges.file, egos.file = NULL, 
                        ID.vars = list(ego="EgoID", alter="Alter.Number", source="Alter.1.Number", 
                                       target="Alter.2.Number"), ego.vars = NULL, ...) {
  # Preliminaries - Get ID var Names
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)

  # Import CSVs
  alters.df <- read.csv(file = alter.file)
  edges <- read.csv(file = edges.file)
  if (!is.null(egos.file)) { egos <- read.csv(file = egos.file) 
   } else {
    # Extract ego data
    alterID.index <- which(names(alters.df) == IDv$alter)
    egos <- unique(alters.df[1:(alterID.index - 1)])
    
    # Extract alter data
    egoID.index <- which(names(alters.df) == IDv$ego)
    alters.df <- cbind(
                    alters.df[c(alterID.index, egoID.index)], 
                    alters.df[-c(alterID.index, egoID.index)])
  }
  threefiles_to_egor(egos = egos, alters.df = alters.df, edges = edges, ID.vars = IDv, ego.vars = ego.vars, ...)
}


#' @describeIn threefiles_to_egor This function reads in data created
#' by the openeddi survey software and transforms it to an egoR object. If no 
#' parameters are provided \code{read_openeddi} will try to find the adequate files
#' in the working directory. By default the standard ID variable names of openeddi
#' are used, if you need to specify the ID variable names use the ID.vars parameter.
#' Further Information: www.openeddi.com
#' @importFrom utils read.csv
#' @export
read_openeddi <- function(egos.file =  NULL, 
                          alters.file = NULL, 
                          edges.file = NULL, 
                          ID.vars = list(ego="puid",alter="nameid",source="nameid",target="targetid"), 
                          ego.vars = NULL, ...){
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
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
  
  threefiles_to_egor(egos = egos, alters.df = alters.df, edges = edges, ID.vars = IDv, ego.vars = ego.vars, ...)
}


