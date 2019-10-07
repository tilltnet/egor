#' A selective subset of GSS 2004 data
#'
#' This is a selective subset of General Social Survey 2004 data containing
#' variables from network questions. See Details for description how this
#' particular subset was selected. The data has a near 0 research value, it is
#' provided to illustrate the functions in \pkg{egor} package.
#' 
#' @name gss2004
#' 
#' @format A tibble with 499 rows and the variables listed below. Data was
#'   imported from SPSS file and are labelled.
#'   Functions in the \pkg{labelled} package can be used to handle them.
#'   
#'   Variables:
#' \describe{
#'   \item{id}{Case ID}
#'   \item{vpsu, vstrat, wtssall}{Design variables and weight}
#'   \item{age}{Ego's age in years}
#'   \item{race}{Ego's race. 1=white, 2=black, 3=other}
#'   \item{sex}{Ego's sex. 1=male, 2=female}
#'   \item{marital}{Ego's marital status. 1=married, 2=widowed, 3=divorced, 4=separated, 5=never married}
#'   \item{numgiven}{Number of alters mentioned}
#'   \item{age\[1-5\]}{Alter's age in years}
#'   \item{race\[1-5\]}{Alter's race. 1=asian, 2=black, 3=hispanic, 4=white, 5=other}
#'   \item{sex\[1-5\]}{Alter's sex. 1=male, 2=female}
#'   \item{spouse\[1-5\]}{Whether alter is a spouse of ego. 1=mentioned, 2=not mentioned}
#'   \item{close\[1-4\]\[2-5\]}{How close are the two alters according to ego. 1=especially close, 2=know each other, 3=total strangers}
#' }
#' 
#' @details 
#' This dataset was created from original GSS 2004 data for illustrative
#' purposes such that (1) it is small and (2) contains just enough variation in
#' respondent's personal networks to illustrate various functions in the
#' package. It is essentially a stratified sample from original data (1472
#' cases). Strata correspond to groups of cases created from unique combinations
#' of values on the following ego variables: `age` (3 categories), `race`,
#' `sex`, `marital`, `numgiven`. At most 2 cases were sampled from each stratum
#' via simple random sampling with replacement.
#' 
#' 
#' @source General Social Survey data at NORC: \url{http://gss.norc.org/get-the-data}




if(FALSE) {
  ### Code used to make the dataset

  # library(tidyverse)
  # 
  # # Load data from a SPSS file (this was itself a column subset from original SAV from NORC)
  # d <- haven::read_sav("~/Documents/Projects/statnet/ergm.ego-papers/data/GSS/GSS2004net.sav")
  # 
  # 
  # # Select variables
  # 
  # sel <- d %>%
  #   structure(names=tolower(names(.))) %>%
  #   # Variable selection
  #   select(
  #     id, vpsu, vstrat, wtssall, # Design variables
  #     age, race, sex, # Ego attributes  available for alters
  #     marital, # Ego attributes not available for alters
  #     numgiven,
  #     matches("age[1-5]"), matches("race[1-5]"), matches("sex[1-5]"), # Alter attributes available for egos
  #     matches("spouse[1-5]"), # Alter attributes, but dyadic covariates
  #     matches("close[1-4][2-5]") # Alter-alter ties
  #   ) %>%
  #   # Additional vars for case selection
  #   mutate(
  #     .age = cut(age, 3) %>% as.numeric() # groups of age
  #   )
  # 
  # 
  # 
  # # Stratified sample of cases ----------------------------------------------
  # 
  # # Strata correspond to interesting response patterns
  # 
  # svars <- rlang::exprs(.age, race, sex, marital, numgiven)
  # 
  # stratas <- 
  #   sel %>%
  #   group_by(!!!svars) %>%
  #   summarise(n=n()) %>%
  #   ungroup() %>%
  #   mutate(
  #     size = ifelse(n < 2, n, 2)
  #   )
  # 
  # summary(stratas)
  # 
  # 
  # set.seed(666)
  # 
  # samp <- sel %>%
  #   arrange(!!!svars) %>%
  #   as.data.frame(stringsAsFactors=FALSE) %>%
  #   sampling::strata(
  #     stratanames = as.character(svars),
  #     size = stratas$size,
  #     method = "srswor"
  #   ) %>%
  #   as_tibble()
  # 
  # 
  # gss2004 <- sel %>% 
  #   select(-.age) %>%
  #   filter(row_number() %in% samp$ID_unit)
  # 
  # 
  # save(gss2004, file=here::here("data/gss2004.rda"))
  # 
  # 
  # # Some checks
  # gss2004 %>%
  #   select(matches("spouse[1-5]")) %>%
  #   gather(var, val) %>%
  #   distinct(val)
  
  
  
  
}
