test_that("twofiles_to_egor can handle alters without aaties", {
  path_to_alters_8.csv <- system.file("extdata", "alters_8.csv", package = "egor")
  path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
  egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
  alters_8 <- read.csv2(path_to_alters_8.csv, row.names = 1)
  
  egos_8 <- 
    egos_8 %>% 
    as_tibble() %>% 
    mutate_at(vars(X1.to.2:X7.to.8),
              ~ifelse(egoID == 3, NA, .))
  
  attr.start.col <- which(names(egos_8) == "alter.sex.1")
  attr.end.col <- which(names(egos_8) == "alter.age.8")
  dy.first.var <- which(names(egos_8) == "X1.to.2")
  
  expect_error(
    e <- twofiles_to_egor(
      egos = egos_8,
      alters = alters_8,
      e.max.alters = 8,
      e.first.var = dy.first.var),
    NA)
})


test_that("twofiles_to_egor creates alter ID if none is present", {
  path_to_alters_8.csv <- system.file("extdata", "alters_8.csv", package = "egor")
  path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
  egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
  alters_8 <- read.csv2(path_to_alters_8.csv, row.names = 1)
  
  egos_8 <- 
    egos_8 %>% 
    as_tibble() %>% 
    mutate_at(vars(X1.to.2:X7.to.8),
              ~ifelse(egoID == 3, NA, .))
  
  attr.start.col <- which(names(egos_8) == "alter.sex.1")
  attr.end.col <- which(names(egos_8) == "alter.age.8")
  dy.first.var <- which(names(egos_8) == "X1.to.2")
  
  expect_error(
    e <- twofiles_to_egor(
      egos = egos_8,
      alters = select(alters_8, -alterID),
      e.max.alters = 8,
      e.first.var = dy.first.var),
    NA)
})
