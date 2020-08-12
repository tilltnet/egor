context("test_density.R")

# Test Density
# egor

data(egor32)
of <- make_egor(5, 20)


# Weighted
densities <- ego_density(object = of, weight = "weight", directed = F)

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

# Non-numeric ego ID
test_that("non-numeric egoIDs work with ego_density",{
  of$ego$.egoID[5] <- "otto"
  of$alter$.egoID[of$alter$.egoID == 5] <- "otto"
  of$aatie$.egoID[of$aatie$.egoID == 5] <- "otto"
  expect_error(ego_density(object = of), NA)
})

