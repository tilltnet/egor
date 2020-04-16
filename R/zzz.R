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
