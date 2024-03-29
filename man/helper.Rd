% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{helper}
\alias{helper}
\alias{as_nested_egor}
\alias{alters_by_ego}
\alias{alters_by_ego.egor}
\alias{alters_by_ego.nested_egor}
\alias{aaties_by_ego}
\alias{aaties_by_ego.egor}
\alias{aaties_by_ego.nested_egor}
\alias{dyad.poss}
\alias{sanitize.wide.edges}
\alias{create_edge_names_wide}
\alias{dyads_possible_between_groups}
\alias{din_page_dist}
\title{General helper functions}
\usage{
as_nested_egor(x)

alters_by_ego(x)

\method{alters_by_ego}{egor}(x)

\method{alters_by_ego}{nested_egor}(x)

aaties_by_ego(x)

\method{aaties_by_ego}{egor}(x)

\method{aaties_by_ego}{nested_egor}(x)

dyad.poss(max.alters, directed = FALSE)

sanitize.wide.edges(max.alters)

create_edge_names_wide(x)

dyads_possible_between_groups(x, y, geometric = TRUE)

din_page_dist(x)
}
\arguments{
\item{x}{\code{Numeric}.}

\item{max.alters}{A \code{numeric} giving the maximum number of alters.}

\item{directed}{A \code{logical} value indicating directedness of alter-alter
data.}

\item{y}{\code{Numeric}.}

\item{geometric}{\code{Logical.} Calculate possible dyads for geometric mean?}
}
\description{
Helper functions for ego centered network analysis
}
\section{Functions}{
\itemize{
\item \code{as_nested_egor()}: Converts an egor object to a "legacy" egor object with
nested .alts and .aaties columns.

\item \code{alters_by_ego()}: Splits the alter table into a list of tables
(possibly 0-row) of alters associated with each ego, in the same
order as the ego table.

\item \code{aaties_by_ego()}: Splits the alter--alter ties table into a list of
tables (possibly 0-row) of alter--alter associated with each ego, in
the same order as the ego table.

\item \code{dyad.poss()}: Returns the count of possible edges in an
un-directed or directed, ego-centered network, based on the number of alters.

\item \code{sanitize.wide.edges()}: Generates a \code{data.frame} marking possible dyads in
a wide alter-alter relation \code{data.frame}. Row names corresponds to the
network size. This is useful for sanitizing alter-alter relations in the wide
format.

\item \code{create_edge_names_wide()}: Creates a \code{vector} of names for variables
containing data on alter-alter relations/ dyads in ego-centered networks.

\item \code{dyads_possible_between_groups()}: Calculates the possible edges between members of
different groups in an ego-centered network.

\item \code{din_page_dist()}: Calculates the optimal distribution of a number of
equally sized objects on a DIN-Norm DIN 476 (i.e. DIN A4) page in landscape
view.

}}
