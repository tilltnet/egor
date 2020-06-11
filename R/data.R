#' 32 sets of randomly created alters belonging to ego-centered networks
#'
#' @format A data frame with 32 sets of up to 32 alters per egoID and 7 variables:
#' \describe{
#'   \item{.ALTID}{alter identifier}
#'   \item{.EGOID}{ego identifier}
#'   \item{age}{age in categories}
#'   \item{age.years}{age in years}
#'   \item{country}{country}
#'   \item{income}{income}
#'   \item{sex}{gender}
#' }
"alters32"

#' 32 sets of randomly created alter-alter ties belonging to ego-centered networks
#'
#' @format A data frame with 32 sets of alter-alter relations and 4 variables:
#' \describe{
#'   \item{.EGOID}{ego identifier}
#'   \item{.SRCID}{source alter ID}
#'   \item{.TGTID}{target alter ID}
#'   \item{weight}{weight of relation}
#' }
"aaties32"

#' 32 randomly created egos belonging to ego-centered networks
#'
#' @format A data frame with 32 sets of alter-alter relations and 4 variables:
#' \describe{
#'   \item{.EGOID}{ego identifier}
#'   \item{age}{age in categories}
#'   \item{age.years}{age in years}
#'   \item{country}{country}
#'   \item{income}{income}
#'   \item{sex}{gender}
#' }
"egos32"

#' 32 randomly created ego-centered networks stored as an egor object
#'
#' @format An egor object with 32 ego-centered networks (5 variables):
#' \describe{
#'   \item{egoID}{ego identifier}
#'   \item{sex}{ego's gender}
#'   \item{age}{ego's age}
#'   \item{.alts}{nested column/list containing alters}
#'   \item{.aaties}{nested column/list containing alter-alter relations}
#' }
"egor32"

if (FALSE) {
  egor32 <- make_egor(32,32)
  egos32 <- egor32$ego %>% 
    rename_at(vars(starts_with(".")), toupper)
  alters32 <- egor32$alter %>% 
    rename_at(vars(starts_with(".")), toupper)
  aaties32 <- egor32$aatie %>% 
    rename_at(vars(starts_with(".")), toupper)
  usethis::use_data(egor32, egos32, alters32, aaties32, overwrite = TRUE)
}

#' Simulated Allbus 2010 Data
#' 
#' A dataset simulated based on the the original Allbus
#' 2010 SPSS data. The dataset simulates 100 respondents and does not resemble
#' any actual Allbus respondents. Each variable is randomly generated based on
#' the range of the original variables, co-varianaces between variables are
#' disregarded. The data’s purpose is purely to demonstrate how to technically
#' work with the Allbus data using egor and R - no analytical assumptions should
#' be made based on this data! 
#' 
#' The dataset contains (simulated!) answers two ego-centered name generators.
#' 
#' @format A tibble/ data.frame of 100 simulated respondents/ rows and 981 
#' variables/ columns. Each variable is a `labelled dbl`.
"allbus_2010_simulated"
