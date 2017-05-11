# Test Density
# One File
library(egor)
path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)

attr.start.col <- which(names(egos_8) == "alter.sex.1")
attr.end.col <- which(names(egos_8) == "alter.age.8")
dy.first.var <- which(names(egos_8) == "X1.to.2")

of <- read.egonet.one.file(egos_8, egos_8$netsize, 
                           attr.start.col = attr.start.col, 
                           attr.end.col = attr.end.col, 
                           dy.first.var = dy.first.var,
                           dy.max.alteri = 8)
of$edges <- lapply(of$edges, FUN= function(x) transform(x, n_weight = weight/3))

# Weighted
densities <- egoR.density(of, weight = "n_weight", directed = F)

test_that("density values are between 0 an 1", {
  expect_true(max(densities) <= 1)
  expect_true(min(densities) >= 0)
})

# Not Weighted
densities2 <- egoR.density(of, directed = F)

test_that("density2 values are between 0 an 1", {
  expect_true(max(densities2) <= 1)
  expect_true(min(densities2) >= 0)
})