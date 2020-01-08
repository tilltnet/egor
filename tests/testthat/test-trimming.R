context("test-trimming.R")

test_that("trimming works correctly", {
  raw_data_vv <- data.frame(
    V2 = 1:3,
    netsize = sample(1:3, 3, replace = T),
    sex_a = sample(c("m", "f", NA), 3, replace = T),
    sex_b = sample(c("m", "f", NA), 3, replace = T),
    sex_c = sample(c("m", "f", NA), 3, replace = T),
    age_a = sample(c(1:99, NA), 3, replace = T),
    age_b = sample(c(1:99, NA), 3, replace = T),
    age_c = sample(c(1:99, NA), 3, replace = T),
    a_b = c(1,9,1),
    a_c = c(1,9,1),
    c_b = c(1,1,1),
    additional_ego_attr = 1:3
  )
  
  expect_warning(e <- onefile_to_egor(egos = raw_data_vv,
                       ID.vars = list(ego = "V2"),
                       attr.start.col = "sex_a",
                       attr.end.col = "age_c", 
                       aa.first.var = "a_b",
                       max.alters = 3,
                       var.wise = TRUE))
  
  ego1_alters <- 
    e %>% 
    slice(1) %>% 
    activate(alter) %>% 
    as_tibble()
  
  expect_true(all(ego1_alters$.egoID == 1))
  expect_equal(nrow(ego1_alters), 3)
  
  ego1_aaties <- 
    slice(.data = e, 1) %>% 
    activate(aatie) %>% 
    as_tibble()
  
  expect_true(all(ego1_aaties$.egoID == 1))
  expect_equal(nrow(ego1_aaties), 3)
  
  no_altID_2_aaties <- 
    e %>% 
    activate(alter) %>% 
    filter(.altID != 2) %>% 
    activate(aatie) %>% 
    as_tibble()
  
  expect_true(all(no_altID_2_aaties$.srcID != 2))
  expect_true(all(no_altID_2_aaties$.tgtID != 2))
  expect_equal(nrow(no_altID_2_aaties), 3)
})
