#' \code{egor}
#'
#' R Package for importing and analyzing ego-centered-network data.
#'
#' \href{https:///egor.tillt.net/}{Further Information}
#' or \href{https://github.com/tilltnet/egor}{GitHub}
#' 
#' @details When analyzing ego-centered network data it is common to either
#' include or exclude data on ego. By default `egor` excludes ego for most
#' analytic and visual functions and offers to include ego with the argument
#' `include.ego`. In order for egor to automatically detect which ego-level variables
#' correspond to the alter-variables, the variables on the different levels need
#' to be named exactly the same. Alternatively the functions that have an
#' `include.ego` argument have additional arguments that allow to specify the 
#' name of the ego-level variable in question.
#' 
#' @author Till Krenz, \email{egor@tillt.net}
#' @author Pavel Krivitsky, \email{pavel@uow.edu.au}
#' @author Michał Bojanowski \email{m.bojanowski at icm.edu.pl}
#' @author Andreas Herz, \email{herzand@uni-hildesheim.de}
#' @author Raffaele Vacca, \email{r.vacca@ufl.edu}
#' @docType package
#' @name egor-package-doc
NULL

## quiets concerns of R CMD check re: the .'s that appears in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))