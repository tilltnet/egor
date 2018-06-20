context("test_read_three_files.R")

library(egor)

test_that(
  "threefiles_to_egor() works.",
  {
    egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
    alters.file <- system.file("extdata", "alters_32.csv", package = "egor")
    edges.file <-  system.file("extdata", "edges_32.csv", package = "egor")
    
    egos <- read.csv2(egos.file)
    alters <- read.csv2(alters.file)
    edges <- read.csv2(edges.file)
    
    expect_error(
      threefiles_to_egor(egos = egos, alters.df = alters, edges = edges),
      NA)
  }
)
