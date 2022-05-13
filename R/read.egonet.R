if (getRversion() >= "2.15.1") utils::globalVariables(c("alterID", "egoID", ".tmp.srcID", ".tmp.tgtID"))

# Read ego-centered-network data from single file format or two-file format.

#' Obtain the index of a column in a data frame (or a list), producing
#' an error if there is a problem.
#'
#' @param name a character vector giving the names of the columns to look up.
#' @param df a [`data.frame`] or a [`list`] object.
#'
#' @return An integer giving the column index of the named column.
#'
#' @note Numeric inputs for `name` are passed through, so this
#'   function is safe to use if the input is already a column index.
#' @keywords internal
col_idx <- function(name, df) {
  if (is.numeric(name))
    name
  else{
    col <- which(names(df) %in% name)
    if (length(col) != length(name))
      stop("Column ",
           sQuote(name),
           " is not found in ",
           deparse(substitute(df)),
           " or is ambiguous.")
    col
  }
}


#' Longest common prefix of a set of strings.
#'
#' @param x a character vector.
#'
#' @return A character vector that is the longest common substring at
#'   the start of each of the input vectors.
#' @keywords internal
common_prefix <- function(x) {
  j <- 0
  # There is probably a faster way to do this.
  while (length(unique(sapply(x, substr, 1, j + 1))) == 1)
    j <- j + 1
  res <- substr(x[1], 1, j)
  if (nchar(res) == 0) return(x[1])
  res
}

#' Transform 'wide' alter-level data to the 'long'-format
#'
#' A function to transform a wide-format dataframe of ego-centered network data
#' into a long-format data-frame, where every row represents one alter/dyad. In
#' the created dataframe numerous networks can be distinguished by a network ID
#' (egoID).
#' @template wide
#' @template egoID
#' @template max_alters
#' @param start.col Index or name of the first column containing alter-alter relation data.
#' #!# Should: Defaults to first column of \code{wide}.
#' @param last.col Index or name of the first column containing alter-alter relation data.
#' #!# Should: Defaults to last column of \code{wide}.
#' @template ego_vars
#' @param var.wise a logical value indicating whether the alter attributes are
#' stored variable-wise, if FALSE alter-wise storage is assumed.
#' @keywords internal
wide.to.long <-
  function(wide,
           egoID = "egoID",
           max.alters,
           start.col,
           end.col,
           var.wise = FALSE) {
    start.col <- col_idx(start.col, wide)
    end.col <- col_idx(end.col, wide)
    ### Generating a matrix containing all variable names of one particular alters
    ### item (sex, age, etc.).
    mt_dimmer <-
      ifelse(var.wise == TRUE, max.alters, ncol(wide[start.col:end.col]) / max.alters)
    #print(mt_dimmer)
    name_mt <- matrix(names(wide[start.col:end.col]), mt_dimmer)
    #print(name_mt)
    if (var.wise)
      name_mt <- t(name_mt)
    #if(!var.wise) print("var.wise not TRUE")
    
    ### Transfrom Matrix to a list where every entry is a vector of the variables
    ### for one item (sex, age, etc.).
    vary <- list()
    vn <- c()
    
    # Wenn var.wise max.alters, statt alters.item.count nehmen!!! #!#
    for (i in 1:dim(name_mt)[1]) {
      vary[[i]] <- name_mt[i,]
      vn <- c(vn, common_prefix(vary[[i]]))
    }
    
    # Generate a vector giving numbers to the alters (alterID).
    times <- seq_along(vary[[1]])
    
    ### Create a long format data.frame of the alters items.
    coll_df <- wide[start.col:end.col]
    
    #' @importFrom stats reshape
    long <-
      reshape(
        coll_df,
        varying = vary,
        ids = wide[egoID],
        v.names = vn,
        times = times,
        direction = 'long',
        idvar = egoID,
        new.row.names = 1:(NROW(wide) * length(times))
      )
    
    ### Change names of alterID and egoID variables.
    colnames(long)[which(names(long) == "time")] <- "alterID"
    #print(which(names(long) == "id"))
    egoID_idx <- col_idx(egoID, long)
    alterID_idx <- col_idx("alterID", long)
    long <-
      cbind(egoID = long[egoID], alterID = long["alterID"], long[,-c(egoID_idx, alterID_idx)])
    long <- long[order(long[[egoID]], long$alterID), ]
    
    ### Return:
    long
  }


#' Transform wide alter-alter data to an edge list.
#
#' When alter-alter for numerous networks is stored in one file/object it is
#' common use the 'wide' data format. This function transforms such data to an
#' edge lists.
#' @param e.wide A dataframe containing the alter-alter relation data in the
#' 'wide' format.
#' @param fist.var Number of column containing the relation between the first and
#' the second network contact.
#' @param max.alters Maximum number of alters for which alter-alter relations
#' were collected.
#' @keywords internal
wide.dyads.to.edgelist <- function(e.wide,
                                   first.var,
                                   max.alters,
                                   alters.list = NULL,
                                   selection = NULL) {
  first.var <- col_idx(first.var, e.wide)
  
  
  ### Calculate max. possible count of dyads per network.
  dp <- dyad.poss(max.alters)
  
  ### Create a helper matrix for naming alters.
  if (is.null(selection)) {
    name.matrix <- 1:max.alters
    for (i in 1:(max.alters - 1)) {
      start.val <- i + 1
      # c(x:y,rep()) is used to avoid cbind throwing warning because of unequal
      # vector lengths.
      name.matrix <-
        cbind(name.matrix, c(start.val:max.alters, rep(9, i)))
      
    }
  }
  ### Extract relevant variables from dataset.
  last.var <- first.var + dp - 1
  alter.alter <- e.wide[first.var:last.var]
  
  # Create a list of dataframes, each containing the edgelists per network.
  #!# This could probably be done with reshape!?
  alter.alter.list <- list()
  count.var <- 1
  
  for (case in 1:nrow(e.wide)) {
    alter.alter.df <- data.frame()
    count.var <- 1
    if (!is.null(selection)) {
      if (nrow(alters.list[[case]]) != 0) {
        names_ <-
          as.character(subset(alters.list[[case]], alters.list[[case]][[selection]] == 1)$alterID) #!# ['alterID'] ??
        #if(length(names) < max.alters) {
        #  diff_ <- max.alters - length(names_)
        #  names_ <- c(names_, rep("99", diff_))
        #}
      } else {
        names_ <- character(0)
      }
      name.matrix <- names_
      for (i in 1:(max.alters - 1)) {
        start.val <- i + 1
        # c(x:y,rep()) is used to avoid cbind throwing warning because of unequal
        # vector lengths.
        name.matrix <-
          suppressWarnings(cbind(name.matrix, c(names_[start.val:max.alters], rep(99, i))))
      }
    }
    
    i <- 1
    for (i in 1:(max.alters - 1)) {
      for (j in 1:(max.alters - i)) {
        this.alter.alter <- data.frame(
          .tmp.srcID = name.matrix[i, 1],
          .tmp.tgtID = name.matrix[i + 1, j],
          weight = alter.alter[case, count.var][[1]],
          stringsAsFactors = FALSE
        )
        alter.alter.df <-
          rbind(alter.alter.df, this.alter.alter, stringsAsFactors = FALSE)
        count.var <- count.var + 1
        #' @importFrom stats na.omit
        alter.alter.df <- na.omit(alter.alter.df)
        rownames(alter.alter.df) <- c()
      }
      alter.alter.list[[as.character(case)]] <- alter.alter.df
    }
  }
  
  ### Delete all zero edges.
  alter.alter.list2 <- lapply(alter.alter.list, function(x)
    subset(x, x$weight != 0))
  
  ### Return:
  alter.alter.list2
}

if (getRversion() >= "2.15.1")
  utils::globalVariables(c("nm"))
#' Transform wide alter-alter data to an edge list.
#
#' A regex based implementation to convert a wide list to an edgelist.
#'
#' @param e.wide A dataframe containing the alter-alter relation data
#'   in the 'wide' format.
#' @template aa.regex
#' @template netsize
#' @keywords internal
wide.dyads.to.edgelist.regex <-
  function(e.wide, aa.regex, netsize) {
    en <- names(e.wide)
    ms <- gregexpr(aa.regex, en, perl = TRUE)
    
    um <- which(unlist(ms) == -1)
    if (length(um)) {
      warning(
        "Columns ",
        paste(sQuote(en[um]), collapse = ","),
        " did not match the regular expression and were omitted."
      )
      e.wide <- e.wide[-nm]
      if (ncol(e.wide) == 0)
        stop("None of the columns matched the regular expression.")
      en <- names(e.wide)
      ms <- gregexpr(aa.regex, en, perl = TRUE)
    }
    
    cps <- mapply(function(cn, m) {
      ss <- attr(m, "capture.start")
      ls <- attr(m, "capture.length")
      ns <- attr(m, "capture.names")
      mapply(function(s, l, n)
        structure(substr(cn, s, s + l - 1), names = n), ss, ls, ns)
    }, en, ms, SIMPLIFY = FALSE)
    
    col.list <- mapply(function(col, cp) {
      col <-
        data.frame(
          col,
          .egoRow = seq_along(col),
          .tmp.srcID = as.integer(cp["src"]),
          .tmp.tgtID = as.integer(cp["tgt"])
        )
      names(col)[1] <- cp["attr"]
      col
    }, e.wide, cps, SIMPLIFY = FALSE)
    
    col.df <- Reduce(function(x, y) {
      if (names(x)[1] == names(y)[1])
        rbind(x, y)
      else
        merge(x,
              y,
              by = c(".egoRow", ".tmp.srcID", ".tmp.tgtID"),
              all = TRUE)
    }, col.list)
    
    col.df.list <- split(col.df, col.df$.egoRow)
    
    col.df.list <- mapply(function(e, n) {
      e <- e[-col_idx('.egoRow', e)]
      e <- e[e$.tmp.srcID <= n & e$.tmp.tgtID <= n, ]
    }, col.df.list, netsize, SIMPLIFY = FALSE)
  }

#' Import ego-centered network data from 'one file format'
#'
#' This function imports ego-centered network data, stored in a single file, providing
#' ego, alter and edge data. This data format is used by the Allbus 2010 (GESIS)
#' and similar social surveys.
#' @template egos
#' @param netsize Numeric, network size values are used to filter out empty
#' alter entries. If the alter data is not structured in a way, where valid alters
#' are stored before the invalid alters, pass NULL here and filter out invalid
#' alters afterwards.
#' @param ID.vars Character. For onefile_to_egor only the name of the ego ID needs
#' to be provided.
#' @param attr.start.col Index or name of the first column containing alter attributes.
#' @param attr.end.col Index or name of the last column containing alter attributes.
#' @param max.alters Maximum number of alters.
#' @param aa.first.var First column containing alter-alter relations/ edges.
#' @template aa.regex
#' @param var.wise Logical value indicating if the alter attributes are sorted variable wise (defaults to FALSE).
#' @param ... additional arguments to [egor()].
#' @template return_egoR
#' @references Muller, C., Wellman, B., & Marin, A. (1999). How to Use SPSS to
#' Study Ego-Centered Networks. Bulletin de Methodologie Sociologique,
#' 64(1), 83-100.
#' @keywords import
#' @examples
#' path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
#' egos_8 <- read.csv2(path_to_one_file_8)
#'
#' onefile_to_egor(
#'   egos = egos_8, netsize = egos_8$netsize,
#'   attr.start.col = "alter.sex.1",
#'   attr.end.col = "alter.age.8",
#'   aa.first.var = "X1.to.2",
#'   max.alters = 8)
#' @export
onefile_to_egor <-
  function(egos,
           netsize = NULL,
           ID.vars = list(ego = "egoID"),
           attr.start.col,
           attr.end.col,
           max.alters,
           aa.first.var,
           aa.regex = NULL,
           var.wise = FALSE,
           ...) {
    IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
    
    egos <- as.data.frame(egos)
    
    attr.start.col <- col_idx(attr.start.col, egos)
    attr.end.col <- col_idx(attr.end.col, egos)
    aa.first.var <- col_idx(aa.first.var, egos)
    aa.last.var <-  aa.first.var + dyad.poss(max.alters) - 1
    
    #Sort egos by egoID.
    cat("Sorting data by egoID: ")
    egos <- egos[order(as.numeric(egos[[IDv$ego]])), ]
    message("Done.")
    
    cat("Transforming alters data to long format: ")
    alters.df <-
      wide.to.long(
        wide = egos,
        egoID = IDv$ego,
        max.alters = max.alters,
        start.col = attr.start.col,
        end.col = attr.end.col,
        var.wise = var.wise
      )
    message("Done.")
    
    cat("Transforming wide dyad data to edgelist: ")
    e.lists <- if (is.null(aa.regex)) {
      wide.dyads.to.edgelist(e.wide = egos, first.var = aa.first.var,
                             max.alters)
    } else {
      wide.dyads.to.edgelist.regex(e.wide = egos[aa.first.var:aa.last.var],
                                   aa.regex = aa.regex,
                                   netsize = netsize)
    }
    message("Done.")
    message("Note: Make sure to filter out alter-alter ties with invalid weight values.")
    
    elist <-
      purrr::map2_dfr(egos[[IDv$ego]], e.lists, function(ego_id, edges)
      {
        if (nrow(edges) > 0) {
          edges[[IDv$ego]] <- ego_id
          edges$weight <- as.vector(edges$weight)
          edges
        }
      })

    # Filter out alters by network size
    if (!is.null(netsize)) {
      cat("Filtering out empty alter entries using provided network size values: ")
      a <- c(1, cumsum(rep(max.alters, nrow(egos))) + 1)
      a <- a[-length(a)]
      c <- purrr::map2(a,
                       netsize, ~ if ((!is.na(.y)) &
                                      .y != 0)
                         seq(.x, .y + .x - 1)) %>% unlist()
      alters.df <- alters.df[c, ]
      message("Done.")
    } else {
      warning("No netsize values provided, make sure to filter out invalid alter entries.")
    }
    
    # Filter out aaties for egos with no alters
    
    elist <- 
      filter(elist, !!rlang::sym(IDv$ego) %in% unique(alters.df[[IDv$ego]]))
    
    # Filter out aaties that reference non-existing alters
    
    alters.df[[IDv$ego]] <- factor(alters.df[[IDv$ego]])
    elist[[IDv$ego]] <- factor(elist[[IDv$ego]], levels = levels(alters.df[[IDv$ego]]))

    if (!is.null(netsize)) {
      elist <- 
        purrr::map2_dfr(split(alters.df, alters.df[IDv$ego]),
                    split(elist, elist[IDv$ego]),
                    function(alt, aa) {
                      aa <- filter(aa, .tmp.srcID %in% alt$alterID)
                      filter(aa, .tmp.tgtID %in% alt$alterID)
                    })
    }
    
    # Return:
    egor(
      alters = alters.df,
      egos = egos[-c(attr.start.col:attr.end.col, aa.first.var:aa.last.var)],
      aaties = elist,
      ID.vars = list(
        ego = IDv$ego,
        alterID = "alterID",
        source = ".tmp.srcID",
        target = ".tmp.tgtID"
      ),
      alter_design = list(max = max.alters),
      ...
    )
  }

#' Import ego-centered network data from two file format
#'
#' This function imports ego-centered network data, stored in two files, where
#' one file contains the ego attributes and the edge information and the other file
#' contains the alters data. This form of data storage for ego-centered network data
#' is proposed by Muller, Wellman and Marin (1999).
#' @template egos
#' @template alters
#' @template ID.vars
#' @param max.alters Maximum number of alters that are included in edge data.
#' @param aa.first.var Index or name of the first column in \code{egos} containing alter-alter data.
#' @param selection \code{Character} naming \code{numeric} variable indicating
#' alters selection with zeros and ones.
#' @param ... additional arguments to [egor()].
#' @template return_egoR
#' @keywords import
#' @examples
#' path_to_alters_8.csv <- system.file("extdata", "alters_8.csv", package = "egor")
#' path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
#'
#' # read data from disk
#' egos_8 <- read.csv2(path_to_one_file_8)
#' alters_8 <- read.csv2(path_to_alters_8.csv)
#'
#' # convert to egor object
#'   twofiles_to_egor(
#'     egos = egos_8,
#'     alters = alters_8,
#'     max.alters = 8,
#'     aa.first.var = "X1.to.2")
#' @export
twofiles_to_egor <- function(egos,
                             alters,
                             ID.vars = list(
                               ego = "egoID",
                               alter = "alterID",
                               source = "Source",
                               target = "Target"
                             ),
                             max.alters,
                             aa.first.var,
                             selection = NULL,
                             ...) {
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  
  egos <- as.data.frame(egos)
  alters <- as.data.frame(alters)
  
  if (!IDv$alter %in% names(alters)) {
    message(paste0(
      "Alter data has no variable called ",
      IDv$alter,
      ". Generating alter ID."
    ))
    
    alters <- alters[order(alters[[IDv$ego]]), ]
    alters[[IDv$alter]] <-
      unlist(map(rle(alters[[IDv$ego]])$lengths, ~ 1:.))
  }
  
  # Sort egos by egoID and alters by egoID and alterID.
  message("Sorting data by egoID and alterID.")
  egos <- egos[order(egos[[IDv$ego]]), ]
  alters <-
    alters[order(alters[[IDv$ego]], alters[[IDv$alter]]), ]
  
  message("Transforming wide edge data to edgelist.")
  elist <-
    wide.dyads.to.edgelist(
      e.wide = egos,
      first.var = aa.first.var,
      max.alters = max.alters,
      alters.list = split(alters, factor(alters[[IDv$ego]], levels = unique(egos[[IDv$ego]]))),
      selection = selection
    )
  
  elist <-
    purrr::map2_dfr(egos[[IDv$ego]], elist, function(ego_id, edges)
    {
      if (nrow(edges) > 0) {
        edges[[IDv$ego]] <- ego_id
        edges$weight <- as.vector(edges$weight)
        edges
      }
    })
  
  # Check and ensure that tgt/src and .altID are same class
  elist_alters <-
    harmonize_id_var_classes(elist,
                             alters,
                             c(".tmp.tgtID", ".tmp.srcID"),
                             "alterID")
  
  elist <- elist_alters$df1
  alters <- elist_alters$df2
  
  # Return:
  egor(
    alters,
    egos,
    elist,
    ID.vars = list(
      ego = IDv$ego,
      alter = IDv$alter,
      source = ".tmp.srcID",
      target = ".tmp.tgtID"
    ),
    ...
  )
}


# convert to highest class that still captures everything,
# character is lowest (lcc least common class)
create_as_lcc <-
  function(x) {
    classes <- unique(purrr::map_chr(x, function(x)
      is.double(x)))
    if (length(classes) == 1) {
      return(function(x)
        x)
    } else if (any(!purrr::map_lgl(x, is.numeric))) {
      return(as.character)
    } else {
      return(as.numeric)
    }
  }

harmonize_id_var_classes <-
  function(df1, df2, var_names1, var_names2) {
    a <- purrr::map2(list(df1, df2),
              list(var_names1, var_names2),
              ~ purrr::map(.y, function(z)
                .x[[z]]))
    as_lcc <- create_as_lcc(unlist(a, recursive = FALSE))
    list(df1 = mutate(df1, across(all_of(var_names1), as_lcc)),
         df2 = mutate(df2, across(all_of(var_names2), as_lcc)))
  }
