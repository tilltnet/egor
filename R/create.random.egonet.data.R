# Create sample ego-centered network data.


#' Generate a random edge list for one network.
#'
#' Here should be a a longer description of this function.
#' @param max.alteri \code{Numeric} indicating maximum number of alteri.
#' @keywords ego-centric network
#' @keywords internal
generate.sample.edge.list <- function(max.alteri) {
  dp <- dyad.poss(max.alteri)
  
  Source <- c()
  for (i in 1:max.alteri) {
    tmp <- rep(i, max.alteri - i)
    Source <- c(Source, tmp)
  }
  
  Target <- c()
  for (i in 1:(max.alteri - 1)) {
    tmp <- rep((i + 1):max.alteri)
    Target <- c(Target, tmp)
  }
  
  weight <- sample(1:3, dp, replace = T)
  data.frame(Source, Target, weight)
} 

#' Generate random ego-centric-network data.
#'
#' This function generates random ego-centric-network data for a specified number of networks with a maximum network size. The network size of the generated networks is a normal distribution with sd=5.
#' @param net.count Number of networks/ egos to generate.
#' @param max.alteri Maximum size of networks.
#' @param netsize \code{Numeric} for fixed network sizes.
#' @keywords ego-centric network
#' @keywords random
#' @export
generate.sample.ego.data <- function(net.count, max.alteri, netsize = NULL) {
  
  # Generating ego data
  egoID <- 1:net.count
  sex <- chartr("12", "wm", sample(1:2, net.count, replace = T))
  age <- sample(1:7, net.count, replace = T)
  age <- factor(age, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("0 - 17", 
      "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100"))
  
  # Generating netsize
  if (is.null(netsize)) {
    probs <- dnorm(seq(-max.alteri/2, max.alteri/2, length = max.alteri), sd = 5)  
    netsize <- sample(1:max.alteri, net.count, prob = probs, replace = T)
    plot(table(netsize), type="l",ylab = "frequency")
    plot(sort(netsize, decreasing = T), type="l",ylab = "netsize")
  } else if (netsize == 'fixed') {
    netsize <- max.alteri
  }
  
  # Creating egos return object
  egos <- data.frame(egoID, sex, age, netsize)
  
  # Generating alteri data
  alterID <- rep(1:max.alteri, net.count)
  egoID <- gl(net.count, max.alteri)
  alter.sex <- rep(chartr("12", "wm", sample(1:2, net.count, replace = T)), 
                   max.alteri)
  alter.age <- rep(sample(1:7, net.count, replace = T), max.alteri)
  alter.age <- factor(alter.age, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("0 - 17", 
      "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100"))
  alteri <- data.frame(egoID, alterID, alter.sex, alter.age)
  
  # Trimming down alteri per network using netsize
  alteri <- long.df.to.list(alteri, egos$netsize, egoID = "egoID", back.to.df = T)
    
  # Generating edges
  edge.list <- list()
  for (i in 1:net.count) {
    edge.list[[i]] <- generate.sample.edge.list(egos[i,]$netsize)
  }
  
  # Return
  list(egos = egos, alteri = alteri, edges = edge.list)
} 

# mimi <- generate.sample.ego.data(net.count = 128, max.alteri = 8, netsize = 'fixed')
# edges <- mimi$edges
# egos <- mimi$egos
# alteri <- mimi$alteri
# alteri.list <- long.df.to.list(alteri, egos, egos$netsize, egoID = "egoID", back.to.df = F)
# 
# tr_ch <- c()
# for (i in 1:NROW(egos)) {
#   egos_netsize <- egos[i,]$netsize
#   alteri_NROW <- NROW(alteri[alteri$egoID == i, ])
#   edges_src <- length(unique(edges[[i]]$Source)) + 1
#   edges_trgt<- length(unique(edges[[i]]$Target)) + 1
#   print(paste("###", i, sep = ": "))
#   print(c(egos_netsize, alteri_NROW, edges_src, edges_trgt))
#   tr <- sum(egos_netsize, alteri_NROW, edges_src, edges_trgt) == 4 * egos_netsize
#   print(tr)
#   tr_ch <- c(tr_ch, tr)
# }
# all(tr_ch)

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