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

test_that("as.egor converts nested_egor object to egor object", {
  data(egor32)
  
  # ego_design is lost, shouldn't be too much of an issue though
  expect_error(as.egor(x = as_nested_egor(egor32)), NA)
})

test_that("as_nested_egor works with egor that has survey design", {
  data(egor32)
  res <- as_nested_egor(egor32)
  expect_true(".aaties" %in% names(res$variables))
  expect_true(".alts" %in% names(res$variables))
})