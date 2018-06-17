#' \code{egor}
#'
#' R Package for importing and analyzing ego-centered-network data.
#'
#' \href{https://tilltnet.github.io/egor/}{Further Information}
#' or \href{https://github.com/tilltnet/egor}{GitHub}
#' 
#' Thanks to: Martina Morris, Michał Bojanowski
#'
#'
#' @author Till Krenz, \email{egor@tillt.net}
#' @author Pavel Krivitsky, \email{pavel@uow.edu.au}
#' @author Raffaele Vacca, \email{r.vacca@ufl.edu}
#' @author Andreas Herz, \email{herzand@uni-hildesheim.de}
#' @author Christopher McCarty, \email{ufchris@ufl.edu}
#' @author Markus Gamper, \email{m.gamper@uni-koeln.de}
#' @docType package
#' @name egor-package-doc
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))