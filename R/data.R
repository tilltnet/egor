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

#' Transnational personal communities of social support of German migrants in Great Britain
#' 
#' This is an `egor` object derived from a subset of the data of a personal network
#' study on support relationships German migrants living in the UK maintain. The data was collected in 2010 using respondent driven sampling (snowball sampling).
#' While the number of alters the respondents were allowed to enter was not limited, only a random subsample of up to eight alters were selected for the alter name interpreter and alter-alter tie questions.
#' This data set contains the data 
#' for 50 of the originally 234 egos.
#' 
#' The questionnaire used seven name generators:
#' 1. From time to time, people rely on other people's advice and opinions to help them find their way in life better. In the last 12 months, who have you sought advice from when it came to important decisions, for example about your family or work? (emotional)
#' 2. In the last 12 months who has done little jobs and favors for you or helped you, for example in filling in forms or moving home? (instrumental)
#' 3. In the past year, who have you turned to when you felt down and wanted someone to talk to? (emotional)
#' 4. In the last 12 months, who have you borrowed money from? (instrumental)
#' 5. In the past year, who have you spent your free time with or shared a hobby? ( social companionship)
#' 6. In the past year who have you had disagreements or arguments with (e.g. about everyday affairs, money or property)? (conflict)
#' 7. Who has let you know that you can rely on them (e.g. that they will always be there for you if you need help)? (emotional).
#' 
#' @format transnat: an `egor` object of 50 egos.
#' @references 
#' - Herz, A. (2015). Relational constitution of social support in migrants' transnational personal communities. Social Networks, 40 (1), S. 64-74. 
#' - Herz, A. (2012). Strukturen transnationaler sozialer Unterstützung. Springer Fachmedien Wiesbaden.
"transnat"

#' @rdname transnat 
#' @format alter_df: alter `data.frame` of the transnat dataset.
"alter_df"

#' @rdname transnat 
#' @format ego_df: ego `data.frame` of the transnat dataset.
"ego_df"

#' Simulated Allbus 2010 Data
#' 
#' A dataset simulated based on the the original Allbus
#' 2010 SPSS data. The dataset simulates 100 respondents and does not resemble
#' any actual Allbus respondents. Each variable is randomly generated based on
#' the range of the original variables, co-variations between variables are
#' disregarded. The data’s purpose is purely to demonstrate how to technically
#' work with the Allbus data using egor and R - no analytical assumptions should
#' be made based on this data! 
#' 
#' The dataset contains (simulated!) answers to two ego-centered name generators.
#' 
#' @format A tibble/ data.frame of 100 simulated respondents/ rows and 981 
#' variables/ columns. Each variable is a `labelled dbl`.
"allbus_2010_simulated"