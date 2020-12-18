UNITS <- c("ego","alter","aatie")

IDVARS <- list(ego = ".egoID", 
               alter = ".altID", 
               source = ".srcID", 
               target = ".tgtID")

UNIT_IDVARS <- list(ego = c("ego"),
                    alter = c("ego", "alter"),
                    aatie = c("ego", "source", "target"))


RESERVED_COLNAMES <- c(".egoRow", 
                       ".altRow", 
                       ".srcRow", 
                       ".tgtRow", 
                       unlist(IDVARS))

egor_default_options <- 
  list(
    egor.results_with_design = FALSE,
    egor.rows_active_level = 5,
    egor.rows_inactive_level = 3,
    egor.active_level_to_top = FALSE
  )

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !names(egor_default_options) %in% names(op)
  options(egor_default_options[toset])
  
  invisible()
}

#' Display names and values of global egor options.
#' @details Currently egor makes use of the following global options. Use
#' `options()` to change values.
#' ## egor.print.rows.active.level
#' - `Numeric`. Amount of rows to display when printing the active level of an `egor` object.
#' ## egor.print.rows.inactive.level
#' - `Numeric`. Amount of rows to display when printing the inactive levels of an `egor` object.
#' ## egor.print.switch.active.level.to.top
#' - `Logical`. When printing an egor object, should the active data-level always 
#' be printed first?
#' ## egor.return.results.with.design
#' - `Logical`. `egor` functions that return ego-level results (e.g. one value per
#'  ego) return a `svy_tbl` object containing the `ego_design()`, when this is set to `TRUE`.
#' @export
egor_options <-
  function() {
    op <- options()
    op[names(egor_default_options)]
  }

