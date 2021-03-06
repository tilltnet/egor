% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_methods.R
\name{append_egor}
\alias{append_egor}
\alias{append_rows}
\alias{append_cols}
\title{Append rows/columns to ego, alter or aatie data}
\usage{
append_rows(.egor, ..., .id = NULL)

append_cols(.egor, ...)
}
\arguments{
\item{.egor}{An \code{egor} object.}

\item{...}{Data frames to combine.}

\item{.id}{Data frame identifier.}
}
\value{
\code{egor} object containing the additional rows/ columns on the active level.
}
\description{
These work like dplyr's bind_cols() and bind_rows(). The first
argument has to be an egor object. Additional rows/columns are added bottom/RHS
of the active data level (ego, alter, aatie).
}
\examples{
e <- make_egor(12, 15)

# Adding a column to the ego level
additional_ego_columns <-
  tibble(x = sample(1:3, 12, replace = TRUE))
  
append_cols(e, additional_ego_columns)

# Adding rows to the ego and alter level
additional_ego_rows <-
  list(
    .egoID = 13,
    sex = "w",
    age = factor("56 - 65"),
    age.years = 60,
    country = "Australia"
  ) \%>\%
  as_tibble()
  
additional_alter_rows <-
  list(
    .altID = 1:5,
    .egoID = rep(13, 5),
    sex = sample(c("f", "m"), 5, replace = TRUE)
  ) \%>\%
  as_tibble()
  
append_rows(e, additional_ego_rows) \%>\%
  activate(alter) \%>\%
  append_rows(additional_alter_rows)
}
