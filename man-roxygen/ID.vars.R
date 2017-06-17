#' @param ID.vars A named list containing column names of the relevant
#'   input columns: \describe{
#'
#' \item{`ego`}{unique identifier associated with each ego, defaulting
#'   to `"egoID"`; has no effect if `alters.df` and `aaties.df` are
#'   both lists of data frames.}
#' 
#' \item{`alter`}{unique-within-ego identifier associated with each
#'   alter, defaulting to `"alterID"`; optional `aaties.df` are not
#'   provided.}
#'
#' \item{`source`}{if `aaties.df` is provided, the column given the
#'   alter identifier of the origin of a relation.}
#'
#' \item{`target`}{if `aaties.df` is provided, the column given the
#'   alter identifier of the destination of a relation.}
#'
#' }
