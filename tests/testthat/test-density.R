context("test_density.R")

# Test Density
# egor

data(egor32)
of <- make_egor(5, 20)


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

