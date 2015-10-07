# Small helper functions

#' Helper functions for ego centric network analysis
#'
#' @template max_alteri
#' @template directed
#' @return \code{dyad.poss} returns the count of possible edges.
#' @export
dyad.poss <- function(max.alteri, directed = F) {
  dp <- (max.alteri^2-max.alteri)
  if (!directed) {
    dp <- dp/2 
  }
  dp
}

#' @describeIn dyad.poss Generates a \code{data.frame} marking possible dyads in 
#' a wide alter-alter relation \code{data.frame}. Row names corresponds to the 
#' network size. This is useful for sanitizing alter-alter relations in the wide 
#' format.
#' @export
poss.dyads.wide <- function(max.alteri) {
  x <- max.alteri
  dp <- dyad.poss(x)
  names_ <- c()
  for(i in 1:(x-1)){
    for(j in (i+1):x){
      names_ <- c(names_, paste(i,j,sep=" to "))
    }
  }
  
  m <- matrix(0, nrow = x, ncol = x)
  m[lower.tri(m)] <- 99
  m1 <- m[, 1:(x-1)]
  for(i in 2:x){
    m1 <- cbind(m1, m[, i:(x-1)])
  }
  m1 <- m1[, 1:dp]
  
  df <- data.frame(m1)
  names(df) <- names_
  df
}