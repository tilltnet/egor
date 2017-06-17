# Create sample ego-centered network data.


#' Generate a random edge list for one network.
#'
#' @param netsize \code{Numeric} indicating maximum number of alters.
#' @keywords ego-centric network
#' @keywords internal
generate.sample.edge.list <- function(netsize) {
  dp <- egor:::dyad.poss(netsize)
  Source <- c()
  for (i in 1:netsize) {
    tmp <- rep(i, netsize - i)
    Source <- c(Source, tmp)
  }
  
  Target <- c()
  for (i in 1:(netsize - 1)) {
    tmp <- rep((i + 1):netsize)
    Target <- c(Target, tmp)
  }
  
  weight <- sample((1:3)/3, dp, replace = T)
  data.frame(Source, Target, weight)
} 

#' Generate random ego-centric-network data.
#'
#' This function generates random ego-centric-network data for a specified number of networks with a maximum network size. The network size of the generated networks is a normal distribution with sd=5.
#' @param net.count Number of networks/ egos to generate.
#' @param max.alters Maximum size of networks.
#' @param netsize \code{Numeric} for fixed network sizes.
#' @param plot whether to plot the network size distribution.
#' @keywords ego-centric network
#' @keywords random
#'
#' @export
generate.sample.ego.data <- function(net.count, max.alters, netsize = NULL, plot=FALSE) {
  
  # Generating ego data
  egoID <- as.factor(1:net.count)
  sex <- chartr("12", "wm", sample(1:2, net.count, replace = T))
  age <- sample(1:7, net.count, replace = T)
  age <- factor(age, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("0 - 17", 
      "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100"))
  
  # Generating netsize
  if (is.null(netsize)) {
#' @importFrom stats dnorm
    probs <- dnorm(seq(-max.alters/2, max.alters/2, length = max.alters), sd = 5)  
    netsize <- sample(2:max.alters, net.count, prob = probs[-1], replace = T)
#' @importFrom graphics plot
    if(plot){
      plot(table(netsize), type="l",ylab = "frequency")
      plot(sort(netsize, decreasing = T), type="l",ylab = "netsize")
    }
  } else if (netsize == 'fixed') {
    netsize <- max.alters
  }
  
  # Creating egos return object
  egos <- data.frame(egoID, sex, age)
  
  # Generating alters data
  alterID <- rep(1:max.alters, net.count)
  egoID <- gl(net.count, max.alters)

  alterID <- rep(1:max.alters, net.count)
  egoID <- gl(net.count, max.alters)

  alter.sex <- rep(chartr("12", "wm", sample(1:2, net.count, replace = T)), 
                   max.alters)
  alter.age <- rep(sample(1:7, net.count, replace = T), max.alters)
  alter.age <- factor(alter.age, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("0 - 17", 
      "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100"))

  alters <- data.frame(egoID, alterID, alter.sex, alter.age)
  
  # Trimming down alters per network using netsize
  alters <- long.df.to.list(alters, netsize, egoID = "egoID", back.to.df = T)

  # Generating edges
  edge.list <- list()
  for (i in 1:net.count) {
    edge.list[[i]] <- generate.sample.edge.list(netsize[i])
  }
  
  aaties <- mapply(FUN = function(x, y) data.frame(egoID = y, x), edge.list, as.factor(1:length(edge.list)), SIMPLIFY = F)
  aaties.df <- do.call(rbind, aaties)
  aaties.df <- aaties.df[sample(1:NROW(aaties.df), NROW(aaties.df)/2), ]
  # Return
  egor(alters, egos, aaties.df)
} 

# Used for generating wide edge format data. 
#' Transfroms edge lists to alter-alter wide format data.
#'
#' Only works properly, if the netsize of all networks is constant.
#' @param edges List of \code{data.frames} containg edge lists.
#' @keywords ego-centric network
#' @keywords internal
edges.to.wide <- function(edges) {
  wide_edges <- plyr::ldply(edges, .fun= function(x) t(x$weight))
  names(wide_edges) <- paste(edges[[1]]$Source, "to", edges[[1]]$Target)
  wide_edges
}
