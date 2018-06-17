context("test_density.R")

# Test Density
# egor
library(egor)
library(tibble)
library(testthat)
data(egor32)
of <- egor32
#of <- egor:::make_egor(net.count = 32, max.alters = 16)


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

# ego_density.list 
# 
# of2 <- as_tibble(of)
# 
# alters <- tidyr::unnest(dplyr::select(of2, .alts), .id = "egoID")
# alters <- arrange(alters, egoID)
# aaties <- tidyr::unnest(dplyr::select(of2, .aaties), .id = "egoID")
# aaties <- arrange(aaties, egoID)
# split(alters, alters$egoID)[[31]]
# split(aaties, aaties$egoID)[[31]]
# 
# 
# aaties %>%
#   filter(egoID == 8)
# densities <- ego_density(object = alters, aaties = aaties, weight = "weight")
# 
# test_that("density values are between 0 an 1", {
#   expect_true(max(densities) <= 1)
#   expect_true(min(densities) >= 0)
# })
# 
# # Not Weighted
# densities2 <- ego_density(alters, aaties)
# 
# test_that("density2 values are between 0 an 1", {
#   expect_true(max(densities2) <= 1)
#   expect_true(min(densities2) >= 0)
# })
# 
