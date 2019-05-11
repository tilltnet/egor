#' egor - a data class for ego-centered network data.
#'
#' The function [egor()] is used to create an egor object from
#' ego-centered network data.
#' @param alters.df either a \code{data.frame} containing the alters
#'   (whose nominator is identified by the column specified by `egoID`
#'   or a list of data frames with the same columns, one for each ego,
#'   with empty data frames or `NULL`s corresponding to egos with no
#'   nominees.
#' @param egos.df \code{data.frame} containing the egos.
#' @param aaties.df \code{data.frame} containing the alter-alter
#'   relations in the style of an edge list, or a list of data frames
#'   similar to `alters.df`.
#' @template ID.vars
#' @param ego_design A [`list`] of arguments to [survey::svydesign()]
#'   specifying the sampling design for the egos. If formulas, they
#'   can refer to columns of `egos.df`.
#' @param alter_design A [`list`] of arguments specifying nomination
#'   information. Currently, the following elements are supported:
#'   \describe{\item{\code{"max"}}{Maximum number of alters that an
#'   ego can nominate.}}
#' @template meth_dots
#' 
#' @details If parameters `alters.df`, `egos.df`, and `aaties.df` are
#'   data frames, they need to share a common ego ID variable, with
#'   corresponding values. If `alters.df` and `aaties.df` are lists of
#'   data frames, `egoID` is ignored and they are matched positionally
#'   with the rows of `egos.df`. Of the three parameters only
#'   `alters.df` is necessary to create an `egor` object, and
#'   `egos.df` and `aaties.df` are optional.
#' @note Column names `.alts`, `.aaties`, and `.egoRow` are reserved
#'   for internal use of `egor` and should not be used to store
#'   persistent data. Other `.`-led column names may be reserved in
#'   the future.
#' @return Returns an [`egor`] object. An [`egor`] object is a [`tibble`] whose
#'   top-level columns store the ego attributes, and which has two
#'   special nested columns: `.alts`, containing, for each row (ego) a
#'   table of that ego's alter attributes and `.aaties`, a table
#'   containing that ego's alter--alter ties, if observed.
#'
#'   If alter-alter ties are observed, `.alts` also has a column
#'   `.altID` giving a unique (within each ego) ID of the alter, by
#'   which the alter can be identified in the `.aaties` table for that
#'   ego. `.aaties`, in turn, has columns `.srcID` and `.tgtID` that
#'   contain the source and the target of the alter-alter relation.
#'
#'   In addition, `egor` has two attributes: `ego_design`, containing an
#'   object returned by [survey::svydesign()] specifying the sampling
#'   design by which the egos were selected and `alter_design`, a
#'   [`list`] containing specification of how the alters were
#'   nominated. See the argument above for currently implemented
#'   settings.
#' @keywords ego-centered network analysis
#' @examples 
#' data("edges32") 
#' data("egos32")
#' data("alters32")
#' 
#' egor(alters.df = alters32, 
#'      egos.df = egos32, 
#'      aaties = edges32)
#' @export
egor <- function(alters.df, egos.df = NULL, aaties.df = NULL, ID.vars=list(ego="egoID", alter="alterID", source="Source", target="Target"), ego_design = list(~1), alter_design = list(max = Inf)) {
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  # FUN: Inject empty data.frames with correct colums in to NULL cells in 
  # $.alts and $.aaties
  inj_zero_dfs <- function(x, y) {
    null_entries <- sapply(x[[y]], is.null)
    zero_df <- x[[y]][!null_entries][[1]][0, ]
    zero_dfs <- lapply(x[[y]][null_entries], FUN = function(x) zero_df)
    x[null_entries, ][[y]] <- zero_dfs
    x
  }

  reserved_cols <- function(x){
    if(is.data.frame(x))
      intersect(names(x), RESERVED_COLNAMES)
    else if(is.list(x))
      unique(unlist(lapply(x, reserved_cols)))
  }
  
  check_reserved_cols <- function(x, forwhat){
    rc <- reserved_cols(x)
    if(length(rc)>0) stop("Table of ",forwhat," has reserved column names: ",paste(sQuote(rc), collapse=", "),".")
  }

  check_reserved_cols(alters.df, "alters")
  egor <-
    if(is.data.frame(alters.df)){
      alters_is_df <- TRUE
      # Create initial egor object from ALTERS
      alters.df[[IDv$ego]] <- as.character(alters.df[[IDv$ego]])
      tidyr::nest_(data = alters.df,
                   key_col = ".alts",
                   names(alters.df)[names(alters.df) != IDv$ego]) # Select all but the IDv$ego column for nesting
    }else{
      alters_is_df <- FALSE
      IDv$ego <- ".egoRow"
      tibble::tibble(.egoRow=seq_along(alters.df),
                     .alts=lapply(alters.df, tibble::as_tibble))
    }

  # Rename the alter ID column to standard name.
  egor$.alts <- lapply(egor$.alts, function(x){
    names(x)[names(x)==IDv$alter] <- ".altID"
    x
  })
  
  # If specified add aaties data to egor
  if(!is.null(aaties.df)){
    check_reserved_cols(aaties.df, "alter-alter ties")
    aaties.tib <- if(is.data.frame(aaties.df)){
        if(length(reserved_cols(aaties.df))) stop("Table of alter-alter ties has reserved column names ",paste(reserved_cols(aaties.df), collapse=", "),".") 
      aaties.df[[IDv$ego]] <- as.character(aaties.df[[IDv$ego]])
        tidyr::nest_(data = aaties.df,
                     key_col = ".aaties",
                     names(aaties.df)[names(aaties.df) != IDv$ego])
      }else{
        tibble::tibble(.egoRow=seq_along(alters.df),
                       .aaties=lapply(aaties.df, tibble::as_tibble))
      }
    
    egor <- dplyr::full_join(egor, aaties.tib, by = IDv$ego)
    egor <- inj_zero_dfs(egor, ".aaties")

    # Rename the source and target ID column to standard names.
    egor$.aaties <- lapply(egor$.aaties, function(x){
      names(x)[names(x)==IDv$source] <- ".srcID"
      names(x)[names(x)==IDv$target] <- ".tgtID"
      x
    })
  }
  
  # If specified add ego data to egor
  if(!is.null(egos.df)){
    check_reserved_cols(egos.df, "egos")
    if(!alters_is_df) egos.df$.egoRow <- seq_len(nrow(egor)) else 
      egos.df[[IDv$ego]] <- as.character(egos.df[[IDv$ego]])

    egor <- dplyr::full_join(tibble::as_tibble(egos.df), egor, by = IDv$ego)
    egor <- inj_zero_dfs(egor, ".alts")
    if(".aaties" %in% names(egor)) egor <- inj_zero_dfs(egor, ".aaties")

  }
  
  # Check If IDv$egos valid
  if (length(unique(egor[[IDv$ego]])) < length(egor[[IDv$ego]]))
    warning(paste(IDv$ego, "values are note unique. Check your 'egos.df' data."))

  if(!alters_is_df) egor$.egoRow <- NULL
  
  # Add meta attribute
  #  ----><----

  # Add design information.

  attr(egor, "ego_design") <- .gen.ego_design(egor, ego_design, 2)

  # TODO: Implement name expansion/checking, possibly an S3 class.
  attr(egor, "alter_design") <- alter_design
  
  class(egor) <- c("egor", class(egor))
  egor <- list(egos = select(egor, -.alts, -.aaties),
               alters = as_alts_df(egor),
               aaties = as_aaties_df(egor)
               )
  class(egor) <- c("egor", class(egor))
  activate(egor, "egos")
}

#' Methods to print and summarize [`egor`] objects
#'
#' @param object,x an [`egor`] object.
#' @param ... additional arguments, either unused or passed to lower-level functions.
#' 
#' @docType methods
#' @method summary egor
#' @export
summary.egor <- function(object, ...) {
  # Network count
  nc <- nrow(object)
  
  # Average netsize
  nts <- survey::svymean(unlist(lapply(object$.alts, FUN = NROW)), 
                          ego_design(object))
  
  # Total number of alters
  alts_count <- sum(unlist(lapply(object$.alts, FUN = NROW)))
  
  # Average density
  if(".aaties" %in% names(object)) 
    dens <- survey::svymean(ego_density(object), ego_design(object), na.rm = TRUE) 
  else 
    dens <- NULL
  
  cat(paste(nc, "Egos/ Ego Networks", 
            paste("\n", alts_count, "Alters"),
            "\nAverage Netsize", nts, "\n"))
  if(!is.null(dens)) cat(paste("Average Density", dens))

  # Meta Data
  cat("\nEgo sampling design:\n")
#' @importFrom utils capture.output
  writeLines(paste("  ", capture.output(print(attr(object, "ego_design"))), sep=""))

  cat("Alter survey design:\n")
  cat("  Maximum nominations:", attr(object, "alter_design")$max,"\n")
}

#' @rdname summary.egor
#' @export
#' @method print egor
#' @import tibble
#' @importFrom dplyr group_vars
print.egor <- function(x, ...) {
  cat(paste0("Active tibble: ", attr(x, "active"), "\n"))
  purrr::walk2(x, c("egos: ", "alters: ", "aaties: "), ~{
    cat(.y)
    tibble:::print.tbl(.x, n = 3)
    })
}

#' @rdname egor
#' @param x an object to be coerced to [`egor`].
#' @export
as.egor <- function(x, ...) UseMethod("as.egor")

#' @export
#' @noRd
as.egor.egor <- function(x, ...) x


#' @method as_tibble egor
#' @export
as_tibble.egor <- function(x, ...){
  # There's probably a less kludgy way to do this.
  class(x) <- class(x)[-seq_len(which(class(x)=="egor"))]
  #as_tibble(x)
  x
}

#' @method as.tibble egor
#' @export
as.tibble.egor <- as_tibble.egor
