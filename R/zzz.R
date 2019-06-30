UNITS <- c("ego","alter","aatie")

IDVARS <- list(ego = ".egoID", 
               alter = ".altID", 
               source = ".srcID", 
               target = ".tgtID")

RESERVED_COLNAMES <- c(".egoRow", 
                       ".altRow", 
                       ".srcRow", 
                       ".tgtRow", 
                       unlist(IDVARS))
