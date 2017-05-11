# Network Visualisation Wizzard

#' Network Visualisation Wizzard (egoR)
#'
#' This function uses an 'egoR' object (list of data objects created by an 
#' egor import function), extracts the graphs object and uses it to visualise
#' all networks contained in an interactive Browser Application (R-Shiny).
#' @param egoR A list of six data objects, created by one of egor's import 
#' functions.
#' @return Opens an interactive Browser Application.
#' @examples 
#' data("egoR32")
#' egoR.vis.wizzard(egoR32)
#' @keywords ego-centric network analysis
#' @export
egoR.vis.wizzard <- function(egoR) {
  graphs_ <<- egoR$graphs
  results_ <<- egoR$results
  shiny::runApp(system.file('wizz', package = 'egor'), launch.browser = T)
}