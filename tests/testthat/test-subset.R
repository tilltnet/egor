test_that("test subset.egor (doc example)", {
  # Generate a small sample dataset
  (e <- make_egor(5, 4))
  
  # First three egos in the dataset
  e[1:3, ]
  
  # Using an external vector
  # (though normally, we would use e[.keep,] here)
  .keep <- rep(c(TRUE, FALSE), length.out = nrow(e$ego))
  expect_error(subset(e, .keep), NA)
})

test_that("test subset.egor works with all units", {
  e1 <- make_egor(5, 25)
  
  expect_error(e1[e1$ego$age.years > 35, ], NA)
  
  expect_error(subset(x = e1, subset = e1$ego$sex == "w", unit = "ego"), NA)
  expect_error(subset(x = e1, subset = e1$alter$sex == "m", unit = "alter"), NA)
  expect_error(subset(e1, e1$aatie$weight > 0.5, unit = "aatie"), NA)
  
})
