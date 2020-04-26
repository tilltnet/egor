test_that("as_nested_egor works", {
  expect_error(as_nested_egor(make_egor(5,5)), NA)
})

test_that("as_nested_egor can be reversed to egor", {
  old_egor_obj <- as_nested_egor(make_egor(5,5))
  old_egor_obj <- old_egor_obj %>% 
    mutate_at(vars(.alts, .aaties), ~purrr::map(., function(x) select(x, -.egoID)))
    
  alts <- old_egor_obj %>%
    select(.egoID, .alts) %>%
    tidyr::unnest(.alts)
  
  aaties <- old_egor_obj %>%
    select(.egoID, .aaties) %>%
    tidyr::unnest(.aaties)
  
  egos <- old_egor_obj %>%
    select(-.alts, -.aaties)
  
  expect_error(egor(
    alts,
    egos,
    aaties,
    ID.vars = list(
      ego = ".egoID",
      alter = ".altID",
      source = ".srcID",
      target = ".tgtID"
    )
  ), NA)
})
