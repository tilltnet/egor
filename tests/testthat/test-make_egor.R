test_that("make_egor works", {
  expect_error(make_egor(net.count = 5, max.alters = 5), NA)
})

test_that("edgelist_to_wide works", {
  data("aaties32")
  edges <- 
    split(aaties32, aaties32$.EGOID)
  expect_error(egor:::edgelist_to_wide(edges),
    NA)
})
