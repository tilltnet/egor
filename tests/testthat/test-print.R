test_that("print.egor works with grouped_by()", {
  e <- make_egor(12, 15)
  
  expect_error(e %>% 
    group_by(country), NA)
})

# options(
#   egor.print.rows.active.level = 5,
#   egor.print.rows.inactive.level = 2,
#   egor.print.switch.active.level.to.top = FALSE)
# activate(e, aatie)
# activate(e, alter)
