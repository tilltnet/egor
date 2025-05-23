if (getRversion() >= "2.15.1") utils::globalVariables(c(":="))

#' egor - a data class for ego-centered network data.
#'
#' The function `egor()` is used to create an egor object from
#' ego-centered network data. `as.egor()` converts a list of `igraph`/`network` objects or 
#' a `nested_egor` objects to an `egor` object.
#' @param alters either a \code{data.frame} containing the alters
#'   (whose nominator is identified by the column specified by `egoID`
#'   or a list of data frames with the same columns, one for each ego,
#'   with empty data frames or `NULL`s corresponding to egos with no
#'   nominees.
#' @param egos \code{data.frame} containing the egos.
#' @param aaties \code{data.frame} containing the alter-alter
#'   relations in the style of an edge list, or a list of data frames
#'   similar to `alters.df`.
#' @template ID.vars
#' @templateVar ego_design_name ego_design
#' @template ego_design
#' @param alter_design A [`list`] of arguments specifying nomination
#'   information. Currently, the following elements are supported:
#'   \describe{\item{\code{"max"}}{Maximum number of alters that an
#'   ego can nominate.}}
#' @template meth_dots
#' 
#' @details If parameters `alters.df`, `egos.df`, and `aaties.df` are
#'   data frames, they need to share a common ego ID variable, with
#'   corresponding values. If `alters.df` and `aaties.df` are lists of
#'   data frames, `egoID` is ignored and they are matched by position
#'   with the rows of `egos.df`. Of the three parameters only
#'   `alters.df` is necessary to create an `egor` object, and
#'   `egos.df` and `aaties.df` are optional.
#' @note Column names `.alts`, `.aaties`, and `.egoRow` are reserved
#'   for internal use of `egor` and should not be used to store
#'   persistent data. Other `.`-led column names may be reserved in
#'   the future.
#' @return Returns an [`egor`] object, which is a `named list` with three 
#'    `tibble data.frames`: ego, alter and aatie (alter-alter ties).
#'    Each data set has an `.egoID` column, that groups the data belonging to one
#'    ego. Additionally the alter data has an `.alterID` column, that links to 
#'    the columns `.srcID` and `.tgtID` in the alter-alter tie data.
#'
#'   In addition, `egor` has two attributes: `ego_design`, containing an
#'   object returned by [srvyr::as_survey_design()] specifying the sampling
#'   design by which the egos were selected and `alter_design`, a
#'   [`list`] containing specification of how the alters were
#'   nominated. See the argument above for currently implemented
#'   settings.
#' @keywords ego-centered network analysis
#' @seealso [as_tibble()] for extracting ego, alter, and alter--alter tables, as [`tibble`]s or as `srvyr`'s [`tbl_svy`] surveys.
#' @examples 
#' data("egos32")
#' data("alters32")
#' data("aaties32") 
#' 
#' e <- egor(alters32,
#'           egos32,
#'           aaties32,
#'           ID.vars = list(ego = ".EGOID",
#'                          alter = ".ALTID",
#'                          source = ".SRCID",
#'                          target =  ".TGTID"),
#'           ego_design = alist(strata = sex))
#'
#' e
#'
#' ego_design(e)
#'
#' @export
egor <- function(alters,
                    egos = NULL,
                    aaties = NULL,
                    ID.vars = list(
                      ego = "egoID",
                      alter = "alterID",
                      source = "Source",
                      target = "Target"
                    ),
                    ego_design = NULL,
                    alter_design = list(max = Inf)) {
  
  # Modify ID name list

  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  
  # Make sure that all ID variables are of the same type
  
  ## all numeric?
  
  id_vars <- list(
    alters[[IDv$ego]],
    alters[[IDv$alter]],
    egos[[IDv$ego]],
    aaties[[IDv$ego]],
    aaties[[IDv$source]],
    aaties[[IDv$target]]
  )
  
  id_vars <- id_vars[!is.null(id_vars)]
  
  all_numeric <- 
    all(purrr::map_lgl(id_vars, is.numeric))
  
  ## not all numeric: change all to character
  
  if (!all_numeric & !all(purrr::map_lgl(id_vars, is.character))) {
    alters[[IDv$ego]] <- as.character(alters[[IDv$ego]])
    
    if (!is.null(alters[[IDv$alter]]))
      alters[[IDv$alter]] <- as.character(alters[[IDv$alter]])
    
    if (!is.null(egos)) egos[[IDv$ego]] <- as.character(egos[[IDv$ego]])
    
    if (!is.null(aaties)) {
      aaties[[IDv$ego]] <- as.character(aaties[[IDv$ego]])
      aaties[[IDv$source]] <- as.character(aaties[[IDv$source]])
      aaties[[IDv$target]] <- as.character(aaties[[IDv$target]])
    }

  }

  # Check for reserved column names
  
  check_reserved_colnames <-
    function(x, unit_) {
      if (!is.null(x)) {

        for (idvar in UNIT_IDVARS[[unit_]]) # For each type of IDVAR that the unit has,
          if (IDv[[idvar]] == IDVARS[[idvar]]) # if the user-specified name for that variable is the same as the canonical one,
            RESERVED_COLNAMES <- setdiff(RESERVED_COLNAMES, IDv[[idvar]]) # then it's not a problem if it's in the table.

        if (any(names(x) %in% RESERVED_COLNAMES))
          stop(paste0(
            unit_,
            " dataset uses reserved column name(s): ",
            paste(RESERVED_COLNAMES[RESERVED_COLNAMES %in% names(x)], 
                  collapse = " ")
          ),
          call. = FALSE)
      }
    }
  
  mapply(check_reserved_colnames,
         list(egos, alters, aaties),
         UNITS)

  # Alters
  
  if (!is_tibble(alters)) {
    alters <- as_tibble(alters)
  }

  alters <- select(alters,
                   !!IDVARS$alter := if (!is.null(aaties) || IDv$alter %in% colnames(alters)) !!IDv$alter,
                   !!IDVARS$ego := !!IDv$ego,
                   everything())

  # Egos
  
  if (is.null(egos)) {
    egos <- tibble(.egoID = unique(alters[[IDVARS$ego]]))
  } else {
    if (!is_tibble(egos)) {
      egos <- as_tibble(egos)
    }
    egos <- select(egos,!!IDVARS$ego := !!IDv$ego, everything())
  }
  
  # Alter-Alter
  
  if (is.null(aaties)) {
    if(all_numeric) {
      aaties <- tibble(.egoID = 0, .srcID = 0, .tgtID = 0)[0,]
    } else {
      aaties <- tibble(.egoID = "0", .srcID = "0", .tgtID = "0")[0,]
    }
    
  } else {
    if (!is_tibble(aaties)) {
      aaties <- as_tibble(aaties)
    }
    aaties <- select(
      aaties,
      !!IDVARS$ego := !!IDv$ego,
      !!IDVARS$source := !!IDv$source,
      !!IDVARS$target := !!IDv$target,
      everything()
    )
  }
  
  # Check ID consistency
  
  if (any(duplicated(egos[[IDVARS$ego]])))
    stop("Duplicated ego IDs in `ego` data.",
         call. = FALSE)
  
  if (!all(alters[[IDVARS$ego]] %in% egos[[IDVARS$ego]]))
    stop("There is at least one ego ID in the `alter` data with no corresponding entry in the `ego` data.",
         call. = FALSE)
  
  if (!all(c(aaties[[IDVARS$ego]] %in% egos[[IDVARS$ego]])))
    stop("There is at least one ego ID in the `alter-alter` data with no corresponding entry in the `alter` data.",
         call. = FALSE)

  altID_by_egoID <- split(alters[[IDVARS$alter]] %||% logical(0), factor(alters[[IDVARS$ego]], egos[[IDVARS$ego]]))
  aaID_by_egoID <- map2(split(aaties[[IDVARS$source]], factor(aaties[[IDVARS$ego]], egos[[IDVARS$ego]])),
                        split(aaties[[IDVARS$target]], factor(aaties[[IDVARS$ego]], egos[[IDVARS$ego]])),
                        c)
  alter_aatie_ids_consistent <- purrr::map2_lgl(altID_by_egoID, aaID_by_egoID, function(a, aa) all(aa %in% a))

  if(!all(alter_aatie_ids_consistent))
    stop("There is at least one alter referenced in the `alter-alter` data that is not listed in the `alter` data. Errors were found for egos: ", 
         paste(egos[[IDVARS$ego]][!alter_aatie_ids_consistent], collapse = " "),
         call. = FALSE)
  
  # Return
  
  egor <- list(ego = egos,
               alter = alters,
               aatie = aaties)
  class(egor) <- c("egor", class(egor))
  egor$ego <- .gen.ego_design(egor, ego_design, parent.frame())
  alter_design(egor) <- alter_design
  activate(egor, "ego")
}

#' Methods to print and summarize [`egor`] objects
#'
#' @param object,x an [`egor`] object.
#' @param ... additional arguments, either unused or passed to lower-level functions.
#' @param n Number of rows to print.
#' @docType methods
#' @method summary egor
#' @export
summary.egor <- function(object, ...) {
  
  # Network count
  nc <- nrow(object$ego)
  
  alter_counts <- 
    object$alter |>
    summarise(.netsize = n(), .by = .egoID)
  
  # add zero counts for egos with no alters
  alter_counts <-
    bind_rows(
      alter_counts,
      object |>
        activate(ego) |>
        select(.egoID) |>
        filter(!.egoID %in% alter_counts$.egoID) |>
        mutate(.netsize = 0) |> 
        as_tibble()
    ) |>
    arrange(.egoID)
  
  # Min, Max  & Average netsize
  min_nts <- min(alter_counts$.netsize)
  max_nts <- max(alter_counts$.netsize)
  avg_nts <- mean(alter_counts$.netsize)

  if(has_ego_design(object)) {
    object$ego$variables <- 
      left_join(object$ego$variables, alter_counts, by = ".egoID")
    avg_nts <- survey::svymean(~.netsize, object$ego) |> unname()
  }
  
  # Total number of alters
  alts_count <- nrow(object$alter)
  
  # Average density
  if ("aatie" %in% names(object)) 
    if(has_ego_design(object)) {
      
      errwd_value <- 
        getOption("egor.results_with_design")
      options(egor.results_with_design = TRUE)
      
      dens <- ego_density(object)
      dens <- survey::svymean(~density, dens, na.rm = TRUE)
      
      options(egor.results_with_design = errwd_value)
      
    } else {
      
      dens <- mean(ego_density(object = object)$density, na.rm = TRUE)
      
      } else dens <- NULL
  
  cat(paste(nc, "Egos/ Ego Networks",
            paste0( "\n", alts_count, " Alters"),
            "\nMin. Netsize", min_nts,
            "\nAverage Netsize", avg_nts,
            "\nMax. Netsize", max_nts, "\n"))
  if (!is.null(dens)) cat(paste("Average Density", dens, "\n"))

  # Meta Data
  if(has_ego_design(object)) {
    cat("\nEgo sampling design:\n")
    #' @importFrom utils capture.output
    print(ego_design(object))
  }
  
  cat("Alter survey design:\n")
  cat("  Maximum nominations:", attr(object, "alter_design")$max,"\n")
  
  invisible(tibble(
    stat = c("Networks", "Alters", "min. Netsize", "Average Netsize", "max. Netsize", "Average Density"),
    value = c(nc, alts_count, min_nts, avg_nts, max_nts, dens) |> unname()
  ))
}

#' @rdname summary.egor
#' @param n.active `Numeric`. Number of rows to print for active data level.
#' @param n.inactive `Numeric`. Number of rows to print for inactive data levels.
#' @export
#' @method print egor
#' @import tibble
#' @importFrom dplyr group_vars
print.egor <- function(x,
                       ...,
                       n.active = getOption("egor.rows_active_level"),
                       n.inactive = getOption("egor.rows_inactive_level")) {
  class(x) <- "list"
  active_lgl <- attr(x, "active") == names(x)
  
  if (getOption("egor.active_level_to_top")) {
    data_levels <- c(x[active_lgl],
                     x[!active_lgl])
    active_lgl <- c(TRUE, FALSE, FALSE)
  } else {
    data_levels <- x
  }
  
  # Disable 'Use (...) to see more rows' message
  op <- options(pillar.advice = FALSE)
  
  purrr::pwalk(
    list(data_levels, names(data_levels), active_lgl),
    function(data_level, level_name, active) {
      design <- NULL
      if ("tbl_svy" %in% class(data_level)) {
        data_level <- data_level$variables
        design <- " with survey design"
      }
      summary_row <- pillar::tbl_sum(data_level)
      
      if (is_grouped_df(data_level)) {
        # MB: not tested properly
        summary_row <- paste(summary_row, collapse = " ")
      }
      
      if (active)
        cat(paste0(
          "# ",
          toupper(level_name),
          " data",
          design ,
          " (active): ",
          summary_row,
          "\n"
        ))
      else
        cat(paste0("# ", toupper(level_name), " data", design , ": ", summary_row, "\n"))
      
      print(
        structure(data_level, class=c("egor_tibble", class(data_level))), 
        n = if(active) {
          min(n.active, nrow(data_level))
        } else {
          min(n.inactive, nrow(data_level))
        },
        advice = FALSE
      )
    })
  
  options(op)
  
  invisible(x)
}





# MB: Seemingly the only way to control printing of tibbles is to define a new
# inheriting S3 class... 



#' @export
tbl_sum.egor_tibble <- function(x) {
  NULL
}
