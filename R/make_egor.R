# Create sample ego-centered network data.


#' Generate a random edge list for one network.
#'
#' @param netsize \code{Numeric} indicating maximum number of alters.
#' @keywords ego-centered network
#' @keywords internal
make_edge_list <- function(netsize) {
  dp <- dyad.poss(netsize)
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
  
  weight <- sample((1:3) / 3, dp, replace = TRUE)
  data.frame(Source, Target, weight, stringsAsFactors = FALSE)
}

#' Generate random ego-centered-network data.
#'
#' This function generates random ego-centered-network data for a specified number of networks with a maximum network size. The network size of the generated networks is a normal distribution with sd=5.
#' @param net.count Number of networks/ egos to generate.
#' @param max.alters Maximum size of networks.
#' @param netsize_fixed \code{Logical}, if TRUE  all networks
#' will have max.alters as network size.
#' @param plot whether to plot the network size distribution.
#' @keywords ego-centered network
#' @keywords random
#'
#' @export
make_egor <-
  function(net.count,
           max.alters,
           netsize_fixed = FALSE,
           plot = FALSE) {
    country_names <- c("Poland", "Australia", "USA", "Germany")
    
    # Generating ego data
    egoID <- as.factor(1:net.count)
    sex <- chartr("12", "wm", sample(1:2, net.count, replace = TRUE))
    age_years <- sample(1:100, net.count, replace = TRUE)
    age <- findInterval(age_years, c(0, 17, 26, 36, 46, 56, 66))
    age <-
      factor(
        age,
        levels = c(1, 2, 3, 4, 5, 6, 7),
        labels = c("0 - 17",
                   "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100")
      )
    country <- sample(country_names, net.count, replace = TRUE)
    income <- sample(1:200 * 365, net.count, replace = TRUE)
    
    # Generating netsize
    if (!netsize_fixed) {
      #' @importFrom stats dnorm
      probs <-
        dnorm(seq(-max.alters / 2, max.alters / 2, length = max.alters), sd = 5)
      netsize <-
        sample(2:max.alters,
               net.count,
               prob = probs[-1],
               replace = TRUE)
      #' @importFrom graphics plot
      if (plot) {
        plot(table(netsize), type = "l", ylab = "frequency")
        plot(sort(netsize, decreasing = TRUE),
             type = "l",
             ylab = "netsize")
      }
    } else {
      netsize <- rep(max.alters, net.count)
    }
    
    # Creating egos return object
    egos <-
      data.frame(
        egoID = as.numeric(egoID),
        sex,
        age,
        age.years = age_years,
        country,
        income,
        stringsAsFactors = FALSE
      )
    
    # Generating alters data
    alterID <- rep(1:max.alters, net.count)
    egoID <- gl(net.count, max.alters)
    
    alter.sex <-
      rep(chartr("12", "wm", sample(1:2, net.count, replace = TRUE)),
          max.alters)
    alter.age.years <-
      rep(sample(1:100, net.count, replace = TRUE), max.alters)
    alter.age <- findInterval(alter.age.years, c(0, 17, 26, 36, 46, 56, 66))
    alter.age <-
      factor(
        alter.age,
        levels = c(1, 2, 3, 4, 5, 6, 7),
        labels = c("0 - 17",
                   "18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "66 - 100")
      )
    
    alter.country <-
      rep(sample(country_names, net.count, replace = TRUE), max.alters)
    alter.income <-
      rep(sample(1:200 * 365, net.count, replace = TRUE), max.alters)
    
    alters <- data.frame(
      egoID = as.numeric(egoID),
      alterID,
      sex = alter.sex,
      age = alter.age,
      age.years = alter.age.years,
      country = alter.country,
      income = alter.income,
      stringsAsFactors = FALSE
    )
    
    # Trimming down alters per network using netsize
    a <- cumsum(rep(max.alters, net.count)) - max.alters + 1
    b <- a + netsize - 1
    alters <- alters[unlist(map2(a, b, seq)),]
    
    # Generating edges
    edge.list <- list()
    for (i in 1:net.count) {
      edge.list[[i]] <- make_edge_list(netsize[i])
    }
    
    aaties <-
      mapply(
        FUN = function(x, y)
          data.frame(egoID = y, x, stringsAsFactors = FALSE),
        edge.list,
        1:length(edge.list),
        SIMPLIFY = FALSE
      )
    aaties.df <- do.call(rbind, aaties)
    aaties.df <-
      aaties.df[sample(1:NROW(aaties.df), NROW(aaties.df) / 2),]
    # Return
    egor(alters,
         egos,
         aaties.df)
  }

# Used for generating wide edge format data.
#' Transforms edge lists to alter-alter wide format data.
#'
#' Only works properly, if the netsize of all networks is constant.
#' @param edges List of \code{data.frames} containing edge lists.
#' @keywords ego-centered network
#' @keywords internal
edgelist_to_wide <- function(edges) {
  wide_edges <- plyr::ldply(
    edges,
    .fun = function(x)
      t(x$weight)
  )
  names(wide_edges) <-
    paste(edges[[1]]$Source, "to", edges[[1]]$Target)
  wide_edges
}
