test_that("as_nested_egor works", {
  expect_error(as_nested_egor(make_egor(5,5)), NA)
})

test_that("as_nested_egor can be reversed to egor", {
  old_egor_obj <- as_nested_egor(make_egor(5,5))
  expect_error(as.egor(x = old_egor_obj), NA)
  old_egor_obj <- old_egor_obj %>% 
    mutate_at(vars(.alts, .aaties), ~purrr::map(., function(x) select(x, -.egoID)))
  expect_error(as.egor(x = old_egor_obj), NA)
    
  old_egor_obj_no_aaties <- 
    old_egor_obj %>% 
    select(-.aaties)
  
  expect_error(as.egor(x = old_egor_obj_no_aaties), NA)

})
