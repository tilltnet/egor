# Create sample data #!# Add network size!
## Net Dataframe 
library(descr)

#max.alteri <- 15
#net.count <- 100 
#dp <- dyad.poss(15)

dyad.poss <- function(max.alteri) { (max.alteri^2-max.alteri)/2 }

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
  
  list(net, alteri, edge.list)
}


