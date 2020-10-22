if (getRversion() >= "2.15.1")
  utils::globalVariables(c("dyads"))

#' Count attribute combinations of dyads in ego-centered networks
#'
#' `count_dyads()` counts the attribute combinations of alter-alter ties/ dyads
#' in ego-centered networks. The results can be returned as a wide or long
#' `tibble`/ `data.frame`.
#' @template object
#' @param alter_var_name `Character`, naming the alter variable to use as
#' attribute.
#' @param return_as `Character`, either "wide" (default) or "long".
#' @param prefix `Character`, added in front of variables. Only used if
#' `return_as` is "wide". If `NULL` (default) prefix is automatically generated.
#' @return Wide or long `tibble`/ `data.frame`.
#' @examples
#' data(egor32)
#' count_dyads(object = egor32,
#'             alter_var_name = "country")
#'
#' # Return result as long tibble.
#' count_dyads(object = egor32,
#'             alter_var_name = "country",
#'             return_as = "long")
#' @export
count_dyads <-
  function(object,
           alter_var_name,
           return_as = c("wide", "long"),
           prefix = NULL) {
    if (!alter_var_name %in% names(object$alter))
      stop("`", alter_var_name, "` not found in alter data.", call. = FALSE)
    if (!return_as[1] %in% c("wide", "long"))
      stop("`return_as` has to be either 'wide' or 'long'.", call. = FALSE)
    
    if (is.factor(object$alter[[alter_var_name]]))
      object$alter[[alter_var_name]] <-
        as.character(object$alter[[alter_var_name]])
    
    aaties_df <-
      as_aaties_df(object, include.alter.vars = TRUE)
    
    aaties_df$dyads <-
      purrr::map2_chr(aaties_df[[paste0(alter_var_name, "_src")]],
                      aaties_df[[paste0(alter_var_name, "_tgt")]],
                      ~ paste(sort(c(.x, .y)), collapse = "_"))
    
    res <- count(aaties_df, .egoID, dyads)
    
    if (return_as[1] == "wide") {
      if (is.null(prefix))
        prefix <-
          paste0("dy_", substring(alter_var_name, 1, 3), "_")
      res <- tidyr::pivot_wider(
        res,
        .egoID,
        names_from = "dyads",
        values_from = n,
        names_prefix = prefix,
        values_fill = list(n = 0)
      )
      
      return_results(object, res)
    }
    else
      res
  }
