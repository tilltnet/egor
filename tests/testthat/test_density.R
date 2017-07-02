cat(" \nTestfile  test_density.r \n")

# Test Density
# egor
library(egor)
library(tibble)
of <- generate.sample.ego.data(net.count = 32, max.alters = 16)

# Weighted
densities <- ego_density(of, weight = "weight", directed = F)

test_that("density values are between 0 an 1", {
  expect_true(max(densities) <= 1)
  expect_true(min(densities) >= 0)
})

# Not Weighted
densities2 <- ego_density(of, directed = F)

test_that("density2 values are between 0 an 1", {
  expect_true(max(densities2) <= 1)
  expect_true(min(densities2) >= 0)
})


# list(s)
# Weighted
densities <- ego_density(of$.alts, of$.aaties,  weight = "weight")

test_that("density values are between 0 an 1", {
  expect_true(max(densities) <= 1)
  expect_true(min(densities) >= 0)
})

# Not Weighted
densities2 <- ego_density(of$.alts, of$.aaties)

test_that("density2 values are between 0 an 1", {
  expect_true(max(densities2) <= 1)
  expect_true(min(densities2) >= 0)
})

# data.frame(s)
library(tidyr)
library(dplyr)
#class(of) <- class(of)[2:4] # 
#class(of) <- class(of)[-seq_len(which(class(of)=="egor"))]

of2 <- as_tibble.egor(of)

alters <- tidyr::unnest(dplyr::select(of2, egoID, .alts))
aaties <- tidyr::unnest(dplyr::select(of2, egoID, .aaties))



densities <- ego_density(alters, aaties, weight = "weight")

test_that("density values are between 0 an 1", {
  expect_true(max(densities) <= 1)
  expect_true(min(densities) >= 0)
})

# Not Weighted
densities2 <- ego_density(alters, aaties)

test_that("density2 values are between 0 an 1", {
  expect_true(max(densities2) <= 1)
  expect_true(min(densities2) >= 0)
})
