context("test_read.R")

test_that(
  "onefile_to_egor() works.",
  {
    path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
    egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
    
    attr.start.col <- which(names(egos_8) == "alter.sex.1")
    attr.end.col <- which(names(egos_8) == "alter.age.8")
    dy.first.var <- which(names(egos_8) == "X1.to.2")
    
    expect_error(onefile_to_egor(
      egos = egos_8, netsize = egos_8$netsize, 
      attr.start.col = attr.start.col, 
      attr.end.col = attr.end.col, 
      aa.first.var = dy.first.var,
      max.alters = 8)
      ,NA)
  }
)

test_that(
  "twofiles_to_egor() works.",
  {
    path_to_alters_8.csv <- system.file("extdata", "alters_8.csv", package = "egor")
    path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
    egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
    alters_8 <- read.csv2(path_to_alters_8.csv, row.names = 1)
    
    attr.start.col <- which(names(egos_8) == "alter.sex.1")
    attr.end.col <- which(names(egos_8) == "alter.age.8")
    dy.first.var <- which(names(egos_8) == "X1.to.2")
    
    expect_error(
      twofiles_to_egor(
        egos = egos_8,
        alters = alters_8,
        e.max.alters = 8,
        e.first.var = dy.first.var),
      NA)
  }
)

test_that(
  "read_egonet() works.",
  {
    path_to_edges_folder <- system.file("extdata", "edges_32", package = "egor")
    path_to_alters_folder <- system.file("extdata", "alters_32", package = "egor")
    path_to_egos_32.csv <- system.file("extdata", "egos_32.csv", package = "egor")
    
    expect_error(
      read_egonet(
        egos.file = path_to_egos_32.csv, 
        edge.folder = paste(path_to_edges_folder, "/", sep = ""),
        alter.folder = paste(path_to_alters_folder, "/", sep = ""),
        first.col.row.names = FALSE, csv.sep = ";")
      ,NA)
  }
)


# Test Cases to build
# - Cases without network information
# - egoID as factor/numeric/character
# - one file: var.wise alter attributes
