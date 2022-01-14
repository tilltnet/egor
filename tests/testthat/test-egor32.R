test_that("egor32 has factor vars", {
  expect_s3_class(egor32$ego$variables$sex, "factor")
  expect_s3_class(egor32$ego$variables$age, "factor")
  expect_s3_class(egor32$alter$sex, "factor")
  expect_s3_class(egor32$alter$age, "factor")
})

test_that("egor32: ego-level is active", {
  expect_equal(attr(egor32, "active"), "ego")
})
