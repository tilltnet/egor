test_that("count_dyads works", {
  data("egor32")
  
  expect_error(count_dyads(object = egor32, alter_var_name = "country"), NA)
  
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country",
    prefix = "pre"
  ),
  NA)
  
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country",
    return_as = "long"
  ),
  NA)
  
  # Errors for wrong arguments
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country3",
    return_as = "asd"
  ))
  
  expect_error(count_dyads(
    object = egor32,
    alter_var_name = "country",
    return_as = "asd"
  ))
})
