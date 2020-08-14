test_that("alts_diversity_... works", {
  data("egor32")
  expect_error(alts_diversity_count(object = egor32, alt.attr = "age"), NA)
  expect_error(alts_diversity_entropy(egor32, "age"), NA)
})

