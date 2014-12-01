# Create sample data #!# Add network size!

## Net Dataframe 
#library(descr)

#max.alteri <- 15
#net.count <- 100 
#dp <- dyad.poss(15)

#' Calculate possible dyads for a given number of alteri.
#'
#' Here should be a a longer description of this function.
#' @param alter.folder
#' @param edge.folder
#' @keywords ego-centric network
#' @keywords sna
dyad.poss <- function(max.alteri) { (max.alteri^2-max.alteri)/2 }

#' Generate a random edge list for one network.
#'
#' Here should be a a longer description of this function.
#' @param alter.folder
#' @param edge.folder
#' @keywords ego-centric network
#' @keywords sna
generate.sample.edge.list <-function(max.alteri) {
  dp <- dyad.poss(max.alteri)
  
  Source <- c()
  for(i in 1:max.alteri) {
    tmp <- rep(i, max.alteri - i)
    Source <- c(Source, tmp)
  }
  
  Target <- c()
  for(i in 1:(max.alteri-1)) {
    tmp <- rep((i+1):max.alteri)
    Target <- c(Target, tmp)
  }
  
  weight <- sample(1:3, dp, replace = T)
 data.frame(Source,Target,weight) 
}

#' Generate random ego-centric-network data.
#'
#' This funtion generates random ego-centric-network data for a specified number of networks with a maximum network size.
#' @param net.count Number of networks/ egos to generate.
#' @param max.alteri Maximum size of networks.
#' @keywords ego-centric network
#' @keywords sna
#' @export
generate.sample.ego.data <- function(net.count, max.alteri) {
  netID <- 1:net.count
  sex <- chartr("12", "wm", sample(1:2,net.count, replace = T))
  age <- sample(1:7, net.count, replace = T)
  age <- factor(age, levels=c(1,2,3,4,5,6,7), labels = c("0 - 17", "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100"))
  probs <- abs(jitter(sort(exp(1:max.alteri), T),  1))
  probs[[1]] <- probs[[1]]/3
  netsize <- sample(1:max.alteri, net.count, prob = probs, replace = T)
  net <- data.frame(netID, sex, age, netsize)
  
  alterID <- rep(1:max.alteri, net.count)
  netID <- gl(net.count, max.alteri)
  alter.sex <- rep(chartr("12", "wm", sample(1:2,net.count, replace = T)), max.alteri)
  alter.age <- rep(sample(1:7, net.count, replace = T), max.alteri)
  alter.age <- factor(alter.age, levels=c(1,2,3,4,5,6,7), labels = c("0 - 17", "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100"))
  alteri <- data.frame(netID, alterID, alter.sex, alter.age)
  
  edge.list <- list()
  for(i in 1:max.alteri) {
    edge.list[[i]] <- generate.sample.edge.list(max.alteri)
  }
  
  list(net = net, long = alteri, edges = edge.list)
}


# 
