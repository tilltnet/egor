test_that("ego_constraint works without weight", {
  e1 <- make_egor(32, 22)
  
  expect_error(ego_constraint(object = e1), NA)
})

test_that("ego_constraint works with weight", {
  e1 <- make_egor(32, 22)
  e1 <-
    e1 %>%
    activate(alter) %>%
    mutate(weight = sample(1:3 / 3, nrow(.$alter), replace = T))
  
  expect_error(ego_constraint(object = e1, weight = "weight"), NA)
})
