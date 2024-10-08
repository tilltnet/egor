% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_egor.R, R/egor.R
\name{as.egor}
\alias{as.egor}
\alias{as.egor.nested_egor}
\alias{as.egor.list}
\alias{egor}
\title{egor - a data class for ego-centered network data.}
\usage{
as.egor(x, ...)

\method{as.egor}{nested_egor}(
  x,
  ID.vars = list(ego = ".egoID", alter = ".alterID", source = ".Source", target =
    ".Target"),
  ...
)

\method{as.egor}{list}(x, ego_name = NULL, ...)

egor(
  alters,
  egos = NULL,
  aaties = NULL,
  ID.vars = list(ego = "egoID", alter = "alterID", source = "Source", target = "Target"),
  ego_design = NULL,
  alter_design = list(max = Inf)
)
}
\arguments{
\item{x}{\code{list} of \code{igraph}/\code{network} objects representing ego networks.}

\item{...}{arguments to be passed to methods}

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

\item{ego_name}{\code{character} or \code{numeric} of length one or same length as there are networks. If the \code{igraph}/\code{network} objects don't include egos as a node, set to \code{NULL} (default).}

\item{alters}{either a \code{data.frame} containing the alters
(whose nominator is identified by the column specified by \code{egoID}
or a list of data frames with the same columns, one for each ego,
with empty data frames or \code{NULL}s corresponding to egos with no
nominees.}

\item{egos}{\code{data.frame} containing the egos.}

\item{aaties}{\code{data.frame} containing the alter-alter
relations in the style of an edge list, or a list of data frames
similar to \code{alters.df}.}

\item{ego_design}{A \code{\link{list}} of arguments to \code{\link[srvyr:as_survey_design]{srvyr::as_survey_design()}} specifying
the sampling design for the egos in terms of the ego
variables. Variable names can be referenced as strings, as
one-sided formulas, or using \code{\link[dplyr:select]{dplyr::select()}} syntax. It is
recommended to use \code{\link[=alist]{alist()}} rather than \code{\link[=list]{list()}} to construct this
argument, particularly when using the \code{select()} syntax. Pass
\code{NULL} to set no design.}

\item{alter_design}{A \code{\link{list}} of arguments specifying nomination
information. Currently, the following elements are supported:
\describe{\item{\code{"max"}}{Maximum number of alters that an
ego can nominate.}}}
}
\value{
Returns an \code{\link{egor}} object, which is a \verb{named list} with three
\verb{tibble data.frames}: ego, alter and aatie (alter-alter ties).
Each data set has an \code{.egoID} column, that groups the data belonging to one
ego. Additionally the alter data has an \code{.alterID} column, that links to
the columns \code{.srcID} and \code{.tgtID} in the alter-alter tie data.

In addition, \code{egor} has two attributes: \code{ego_design}, containing an
object returned by \code{\link[srvyr:as_survey_design]{srvyr::as_survey_design()}} specifying the sampling
design by which the egos were selected and \code{alter_design}, a
\code{\link{list}} containing specification of how the alters were
nominated. See the argument above for currently implemented
settings.
}
\description{
The function \code{egor()} is used to create an egor object from
ego-centered network data. \code{as.egor()} converts a list of \code{igraph}/\code{network} objects or
a \code{nested_egor} objects to an \code{egor} object.
}
\details{
If parameters \code{alters.df}, \code{egos.df}, and \code{aaties.df} are
data frames, they need to share a common ego ID variable, with
corresponding values. If \code{alters.df} and \code{aaties.df} are lists of
data frames, \code{egoID} is ignored and they are matched by position
with the rows of \code{egos.df}. Of the three parameters only
\code{alters.df} is necessary to create an \code{egor} object, and
\code{egos.df} and \code{aaties.df} are optional.
}
\section{Functions}{
\itemize{
\item \code{as.egor(nested_egor)}: Can convert (legacy) \code{nested_egor} object to \code{egor} object.

}}
\note{
Column names \code{.alts}, \code{.aaties}, and \code{.egoRow} are reserved
for internal use of \code{egor} and should not be used to store
persistent data. Other \code{.}-led column names may be reserved in
the future.
}
\examples{
data("egos32")
data("alters32")
data("aaties32") 

e <- egor(alters32,
          egos32,
          aaties32,
          ID.vars = list(ego = ".EGOID",
                         alter = ".ALTID",
                         source = ".SRCID",
                         target =  ".TGTID"),
          ego_design = alist(strata = sex))

e

ego_design(e)

}
\seealso{
\code{\link[=as_tibble]{as_tibble()}} for extracting ego, alter, and alter--alter tables, as \code{\link{tibble}}s or as \code{srvyr}'s \code{\link{tbl_svy}} surveys.
}
\keyword{analysis}
\keyword{ego-centered}
\keyword{network}
