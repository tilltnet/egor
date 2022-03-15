#' R Package for importing and analyzing ego-centered-network data
#'
#' @details When analyzing ego-centered network data it is common to either
#'   include or exclude data on ego. By default `egor` excludes ego for most
#'   analytic and visual functions and offers to include ego with the argument
#'   `include.ego`. In order for egor to automatically detect which ego-level
#'   variables correspond to the alter-variables, the variables on the different
#'   levels need to be named exactly the same. Alternatively the functions that
#'   have an `include.ego` argument have additional arguments that allow to
#'   specify the name of the ego-level variable in question.
#'
#'   For further information see 
#'   \href{https:///egor.tillt.net/}{package webpage} or 
#'   \href{https://github.com/tilltnet/egor}{GitHub}.
#'   
#' @keywords internal
#' @aliases egor-package
"_PACKAGE"


# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appears in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
