% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/composition.R
\name{alts_diversity_count}
\alias{alts_diversity_count}
\alias{alts_diversity_entropy}
\title{Calculate diversity measures on an \code{egor} object.}
\usage{
alts_diversity_count(object, alt.attr)

alts_diversity_entropy(object, alt.attr, base = 2)
}
\arguments{
\item{object}{An \code{egor} object.}

\item{alt.attr}{A \code{character} naming the variable containing the alter-attribute.}

\item{base}{\code{Numeric}, base value of logarithm for entropy calculation.}
}
\value{
A \code{tibble} with the ego ID and a \code{numeric} result vector.
}
\description{
\code{alts_diversity_count()} counts the categories of a variable present in the
networks of an \code{egor} object. \code{alts_diversity_entropy()} calculates the Shannon
entropy as a measurement for diversity of an alter attribute.
}
\examples{
data("egor32")
alts_diversity_count(egor32, "age")
alts_diversity_entropy(egor32, "age")
}
\author{
Michał Bojanowski, \email{michal2992@gmail.com}

Till Krenz, \email{egor@tillt.net}
}
\keyword{analysis}
\keyword{ego-centered}
\keyword{network}
