#' Unnest list columns from an egor object
#' 
#' Provieded an egor-object and the name of a nested column to unnest, this 
#' function puts together the nested \code{data.frames} to a 'global' \code{data.frame},
#' while maintaining the ego identifying variable. This is helpful for extracting
#' alters and alter-alter relation data from an egor object.
#' 
#' @param data An \code{egor} object.
#' @param ... Names of columns to unnest. Use bare variable names.
#' @param .drop Should additional list columns be dropped? By default, 
#' unnest will drop them if unnesting the specified columns requires 
#' the rows to be duplicated.
#' @param .id Data frame idenfier - if supplied, will create a new column with 
#' name .id, giving a unique identifer. This is most useful if the list column 
#' is named.
#' @param .sep If non-NULL, the names of unnested data frame columns will 
#' combine the name of the original list-col with the names from nested 
#' data frame, separated by .sep.
#' @details unnerst_egor() is a wrapper for tidyr's unnest function. 
#' @examples
#' data("egor32")
#' # Extract global alters dataframe
#' unnest_egor(egor32, .alts)
#' 
#' # Extract global alter-alter ties dataframe
#' unnest_egor(egor32, .aaties)
#' @export
unnest_egor <- function(data, ..., egoID = "egoID", .drop = NA, .id = NULL, .sep = NULL) {
  dots <- lazyeval::lazy_dots(...)
  data <- tibble::as_tibble(egor32)
  data <- dplyr::select(data, egoID, ...)
  tidyr::unnest_(data, dots, .drop = .drop, .id = .id, .sep = .sep)
}


