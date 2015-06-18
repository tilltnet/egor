# Network Visualisation Wizzard

#' Network Visualisation Wizzard (egoR)
#'
#' This function uses an 'egoR' object (list of data objects created by an 
#' egonetR import function), extracts the graphs object and uses it to visualise
#' all networks contained (Shiny App).
#' @param egoR A list of six data objects, created by one of egonetR's import 
#' functions.
#' @return Opens a shiny app.
#' @keywords ego-centric network analysis
#' @export
egoR.vis.wizzard <- function(egoR) {
  graphs <- egoR$graphs
  shiny::runApp(system.file('wizz', package = 'egonetR'), launch.browser = T)
}


#' Network Visualisation Wizzard
#'
#' This function uses an object named 'graphs' (list of data objects created by an 
#' egonetR import function), extracts the graphs object and uses it to visualise
#' all networks contained (Shiny App).
#' @param egoR A list of six data objects, created by one of egonetR's import 
#' functions.
#' @param max.netsize Optional parameter. Only needed if the density information
#' was only collected for a subsample of all alteri per network.
#' @return returns a \code{vector} of network density values.
#' @keywords ego-centric network analysis
#' @export
vis.wizzard <- function() {
  shiny::runApp(system.file('wizz', package = 'egonetR'), launch.browser = T)
}