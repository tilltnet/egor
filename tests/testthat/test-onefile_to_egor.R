context("test-onefile_to_egor.R")

test_that("onefile_to_egor works with var wise data", {
  # create test data
  skip_on_cran()
  raw_data_vv <- data.frame(
    V2 = 1:3,
    netsize = sample(1:3, 3, replace = T),
    sex_a = sample(c("m", "f", NA), 3, replace = T),
    sex_b = sample(c("m", "f", NA), 3, replace = T),
    sex_c = sample(c("m", "f", NA), 3, replace = T),
    age_a = sample(c(1:99, NA), 3, replace = T),
    age_b = sample(c(1:99, NA), 3, replace = T),
    age_c = sample(c(1:99, NA), 3, replace = T),
    a_b = c(1,9,NA),
    a_c = c(NA,9,NA),
    c_b = c(1,NA,NA),
    additional_ego_attr = 1:3
  )
  
  # with netsize
  e <- onefile_to_egor(egos = raw_data_vv,
                  netsize = raw_data_vv$netsize,
                  ID.vars = list(ego = "V2"),
                  attr.start.col = "sex_a",
                  attr.end.col = "age_c", 
                  aa.first.var = "a_b",
                  max.alters = 3,
                  var.wise = TRUE)
  expect_true("additional_ego_attr" %in% names(as_tibble(e$ego)))
})

test_that("onefile_to_egor works with alter wise data", {
  # create test data
  skip_on_cran()
  raw_data <- data.frame(
    V2 = 1:3,
    netsize = sample(1:3, 3, replace = T),
    sex_a = sample(c("m", "f", NA), 3, replace = T),
    age_a = sample(c(1:99, NA), 3, replace = T),
    sex_b = sample(c("m", "f", NA), 3, replace = T),
    age_b = sample(c(1:99, NA), 3, replace = T),
    sex_c = sample(c("m", "f", NA), 3, replace = T),
    age_c = sample(c(1:99, NA), 3, replace = T),
    a_b = c(1,9,NA),
    a_c = c(NA,9,NA),
    c_b = c(1,NA,NA),
    additional_ego_attr = 1:3
  )
  
  # with netsize
  e <- onefile_to_egor(egos = raw_data,
                  netsize = raw_data$netsize,
                  ID.vars = list(ego = "V2"),
                  attr.start.col = "sex_a",
                  attr.end.col = "age_c", 
                  aa.first.var = "a_b",
                  max.alters = 3,
                  var.wise = FALSE)
  
  expect_true("additional_ego_attr" %in% names(as_tibble(e$ego)))
})


test_that("onefile_to_egor filters out alters by network size", {
  # create test data
  skip_on_cran()
  raw_data <- data.frame(
    V2 = 1:3,
    netsize = sample(1:3, 3, replace = T),
    sex_a = sample(c("m", "f", NA), 3, replace = T),
    age_a = sample(c(1:99, NA), 3, replace = T),
    sex_b = sample(c("m", "f", NA), 3, replace = T),
    age_b = sample(c(1:99, NA), 3, replace = T),
    sex_c = sample(c("m", "f", NA), 3, replace = T),
    age_c = sample(c(1:99, NA), 3, replace = T),
    a_b = c(1,9,NA),
    a_c = c(NA,9,NA),
    c_b = c(1,NA,NA),
    additional_ego_attr = 1:3
  )
  
  expect_warning(onefile_to_egor(egos = raw_data,
                       ID.vars = list(ego = "V2"),
                       attr.start.col = "sex_a",
                       attr.end.col = "age_c", 
                       aa.first.var = "a_b",
                       max.alters = 3,
                       var.wise = FALSE),
                 label = "providing no netsize")
  
  # with netsize
  e <- onefile_to_egor(egos = raw_data,
                       netsize = raw_data$netsize,
                       ID.vars = list(ego = "V2"),
                       attr.start.col = "sex_a",
                       attr.end.col = "age_c", 
                       aa.first.var = "a_b",
                       max.alters = 3,
                       var.wise = FALSE)
  
  expect_true("additional_ego_attr" %in% names(as_tibble(e$ego)))
  expect_equal(group_by(e$alter, .egoID) %>% group_map(~nrow(.)) %>% unlist(),
               raw_data$netsize)
})


# IDv <- list()
# IDv$ego <- "V2"
# 
# # tibble!!
