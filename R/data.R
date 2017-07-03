#' 32 sets of randomly created alters belonging to ego-centered networks
#'
#' @format A data frame with 32 sets of up to 32 alters per egoID and 7 variables:
#' \describe{
#'   \item{egoID}{ego identifier}
#'   \item{ego.sex}{ego's gender}
#'   \item{age}{ego's age}
#'   \item{netsize}{size of ego's network}
#'   \item{alterID}{alter identifier}
#'   \item{alter.sex}{alter's gender}
#'   \item{alter.age}{alter's age}
#' }
"alters32"

#' 32 sets of randomly created alter-alter ties belonging to ego-centered networks
#'
#' @format A data frame with 32 sets of alter-alter relations and 4 variables:
#' \describe{
#'   \item{egoID}{ego identifier}
#'   \item{Source}{source alter ID}
#'   \item{Target}{target alter ID}
#'   \item{weight}{weight of relation}
#' }
"edges32"

#' 32 randomly created egos belonging to ego-centered networks
#'
#' @format A data frame with 32 sets of alter-alter relations and 4 variables:
#' \describe{
#'   \item{egoID}{ego identifier}
#'   \item{sex}{ego's gender}
#'   \item{age}{ego's age}
#'   \item{netsize}{size of ego's network}
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