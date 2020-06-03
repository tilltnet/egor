test_that("print.egor works with grouped_by()", {
  e <- make_egor(12, 15)
  
  expect_error(e %>% 
    group_by(country), NA)
})
