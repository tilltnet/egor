#' @param <%=ego_design_name%> A [`list`] of arguments to [srvyr::as_survey_design()] specifying
#' the sampling design for the egos in terms of the ego
#' variables. Variable names can be referenced as strings, as
#' one-sided formulas, or using [dplyr::select()] syntax. It is
#' recommended to use [alist()] rather than [list()] to construct this
#' argument, particularly when using the `select()` syntax. Pass
#' `NULL` to set no design.
