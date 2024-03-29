% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.egonet.three.files.R
\name{threefiles_to_egor}
\alias{threefiles_to_egor}
\alias{read_egoweb}
\alias{read_openeddi}
\title{Read/ import ego-centered network data from the three files format, EgoWeb2.0 or
openeddi.}
\usage{
threefiles_to_egor(
  egos,
  alters.df,
  edges,
  ID.vars = list(ego = "egoID", alter = "alterID", source = "Source", target = "Target"),
  ego.vars = NULL,
  ...
)

read_egoweb(
  alter.file,
  edges.file,
  egos.file = NULL,
  ID.vars = list(ego = "EgoID", alter = "Alter.Number", source = "Alter.1.Number", target
    = "Alter.2.Number"),
  ego.vars = NULL,
  ...
)

read_openeddi(
  egos.file = NULL,
  alters.file = NULL,
  edges.file = NULL,
  ID.vars = list(ego = "puid", alter = "nameid", source = "nameid", target = "targetid"),
  ego.vars = NULL,
  ...
)
}
\arguments{
\item{egos}{\code{Data frame} containing ego data (egos as cases)}

\item{alters.df}{\code{Data frame} containing alters data (alters in rows),
alters are connected to their ego by an \code{egoID}.}

\item{edges}{\code{Dataframe}. A global edge list, first column is ego ID
variable.
egos.}

\item{ID.vars}{A named list containing column names of the relevant
input columns: \describe{

\item{\code{ego}}{unique identifier associated with each ego, defaulting
to \code{"egoID"}; has no effect if \code{alters.df} and \code{aaties.df} are
both lists of data frames.}

\item{\code{alter}}{unique-within-ego identifier associated with each
alter, defaulting to \code{"alterID"}; optional \code{aaties.df} are not
provided.}

\item{\code{source}}{if \code{aaties.df} is provided, the column given the
alter identifier of the origin of a relation.}

\item{\code{target}}{if \code{aaties.df} is provided, the column given the
alter identifier of the destination of a relation.}

}}

\item{ego.vars}{A \code{data.frame} of alter attributes in the wide format.}

\item{...}{additional arguments to \code{\link[=egor]{egor()}}.}

\item{alter.file}{A character specifiying the filename of the alters data.}

\item{edges.file}{A character specifiying the filename of the edge data.}

\item{egos.file}{A character specifiying the filename of the ego data.}

\item{alters.file}{\code{Character} name of the alters data file.}
}
\value{
An \strong{egor} object is returned. It is a \code{list} of three data frames:
(1) ego: \code{dataframe} of all
egos and their attributes;
(2) alter: \code{dataframe} of all alters;
(3) aatie: \code{dataframe} of alter alter ties/ edges
}
\description{
These functions read ego-centered network data from the three files
format, EgoWeb2.0 or openeddi and transform it to an egoR object. The three
files format consists of an ego file, on alters file and one file containing
the edge data. EgoWeb2.0 and openeddi use variations of this format.
}
\section{Functions}{
\itemize{
\item \code{read_egoweb()}: This function reads in data from
an EgoWeb 2.0 survey and transforms it to an egoR object. If no file name for
the egos file is provided ego data is assumed to be merged with alters data
and it will be extracted by \code{read_egoweb}. By default the
standard ID variable names of EgoWeb are used, if you need to specify the ID
variable names use the ID.vars parameter. Further Information:
github.com/qualintitative/egoweb

\item \code{read_openeddi()}: This function reads in data created
by the openeddi survey software and transforms it to an egoR object. If no
parameters are provided \code{read_openeddi} will try to find the adequate files
in the working directory. By default the standard ID variable names of openeddi
are used, if you need to specify the ID variable names use the ID.vars parameter.
Further Information: www.openeddi.com

}}
\examples{
# The data for read.egonet.threefiles() needs to be loaded with read.csv(), 
# for it to be converted to an egoR object.
egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
alters.file <- system.file("extdata", "alters_32.csv", package = "egor")
edges.file <-  system.file("extdata", "edges_32.csv", package = "egor")

egos <- read.csv2(egos.file)
alters <- read.csv2(alters.file)
edges <- read.csv2(edges.file)

tf <- threefiles_to_egor(egos = egos, alters.df = alters, edges = edges)

# read_egoweb() and read_openeddi() read the files directly from the disk.
\donttest{
# Fetch current working directory
wd <- getwd()

setwd(system.file("extdata", "openeddi", package = "egor"))
oe <- read_openeddi()

setwd(system.file("extdata", "egoweb", package = "egor"))
ew <- read_egoweb(alter.file = "alters_32.csv", edges.file = "edges_32.csv", 
                  egos.file = "egos_32.csv")
                  
# Restore working directory                   
setwd(wd)
}
}
