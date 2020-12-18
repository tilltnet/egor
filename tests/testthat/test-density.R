context("test_density.R")

# Test Density
# egor




# Weighted

test_that("density values are between 0 an 1", {
  of <- make_egor(5, 20)
  densities <- ego_density(object = of, weight = "weight", directed = F)
  
  expect_true(max(densities$density) <= 1)
  expect_true(min(densities$density) >= 0)
})

# Not Weighted

test_that("density2 values are between 0 an 1", {
  of <- make_egor(5, 20)
  densities2 <- ego_density(of, directed = F)
  
  expect_true(max(densities2$density) <= 1)
  expect_true(min(densities2$density) >= 0)
})

# Non-numeric ego ID
test_that("non-numeric egoIDs work with ego_density",{
  of <- make_egor(5, 20)
  of$ego$.egoID[5] <- "otto"
  of$alter$.egoID[of$alter$.egoID == 5] <- "otto"
  of$aatie$.egoID[of$aatie$.egoID == 5] <- "otto"
  expect_error(ego_density(object = of), NA)
})

test_that("ego_density() returns tbl_svy object, when ego_design present", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <-
    sample(1:10 / 10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  options(egor.results_with_design = TRUE)
  res <- ego_density(object = x)
  expect_is(res, "tbl_svy")
})
