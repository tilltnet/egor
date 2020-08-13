test_that("twofiles_to_egor can handle alters without aaties", {
  path_to_alters_8.csv <-
    system.file("extdata", "alters_8.csv", package = "egor")
  path_to_one_file_8 <-
    system.file("extdata", "one_file_8.csv", package = "egor")
  egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
  alters_8 <- read.csv2(path_to_alters_8.csv, row.names = 1)
  
  egos_8 <-
    egos_8 %>%
    as_tibble() %>%
    mutate_at(vars(X1.to.2:X7.to.8),
              ~ ifelse(egoID == 3, NA, .))
  
  attr.start.col <- which(names(egos_8) == "alter.sex.1")
  attr.end.col <- which(names(egos_8) == "alter.age.8")
  dy.first.var <- which(names(egos_8) == "X1.to.2")
  
  expect_error(
    e <- twofiles_to_egor(
      egos = egos_8,
      alters = alters_8,
      e.max.alters = 8,
      e.first.var = dy.first.var
    ),
    NA
  )
})


test_that("twofiles_to_egor creates alter ID if none is present", {
  path_to_alters_8.csv <-
    system.file("extdata", "alters_8.csv", package = "egor")
  path_to_one_file_8 <-
    system.file("extdata", "one_file_8.csv", package = "egor")
  egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
  alters_8 <- read.csv2(path_to_alters_8.csv, row.names = 1)
  
  egos_8 <-
    egos_8 %>%
    as_tibble() %>%
    mutate_at(vars(X1.to.2:X7.to.8),
              ~ ifelse(egoID == 3, NA, .))
  
  attr.start.col <- which(names(egos_8) == "alter.sex.1")
  attr.end.col <- which(names(egos_8) == "alter.age.8")
  dy.first.var <- which(names(egos_8) == "X1.to.2")
  
  expect_error(
    e <- twofiles_to_egor(
      egos = egos_8,
      alters = select(alters_8, -alterID),
      e.max.alters = 8,
      e.first.var = dy.first.var
    ),
    NA
  )
})

test_that("harmonize_id_var_classes works", {
  var_names1 <- c("a", "c")
  var_names2 <- c("e", "c")
  a <- egor:::harmonize_id_var_classes(
    df1 = tibble(a = 1, b = "2", c = "C"),
    df2 = tibble(e = 1, c = 1L),
    var_names1 = var_names1,
    var_names2 = var_names2
  )
  res_classes <- c(purrr::map_chr(a$df1[var_names1], class),
                   purrr::map_chr(a$df2[var_names2], class))
  expect_equal(n_distinct(res_classes), 1)
  var_names1 <- c("a", "b")
  var_names2 <- c("e", "c")
  b <- egor:::harmonize_id_var_classes(
    df1 = tibble(a = 1, b = "2", c = "C"),
    df2 = tibble(e = 1, c = 1L),
    var_names1 = var_names1,
    var_names2 = var_names2
  )
  
  res_classes <- c(purrr::map_chr(b$df1[var_names1], class),
                   purrr::map_chr(b$df2[var_names2], class))
  expect_equal(n_distinct(res_classes), 1)
})

test_that("twofiles_to_egor returns consistent ID classes",
          {
            path_to_alters_8.csv <-
              system.file("extdata", "alters_8.csv", package = "egor")
            path_to_one_file_8 <-
              system.file("extdata", "one_file_8.csv", package = "egor")
            egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
            alters_8 <-
              read.csv2(path_to_alters_8.csv, row.names = 1)
            
            egos_8 <-
              egos_8 %>%
              as_tibble() %>%
              mutate_at(vars(X1.to.2:X7.to.8),
                        ~ ifelse(egoID == 3, NA, .))
            
            attr.start.col <- which(names(egos_8) == "alter.sex.1")
            attr.end.col <- which(names(egos_8) == "alter.age.8")
            dy.first.var <- which(names(egos_8) == "X1.to.2")
            
            
            e <- twofiles_to_egor(
              egos = egos_8,
              alters = alters_8,
              e.max.alters = 8,
              e.first.var = dy.first.var
            )
            expect_equal(class(e$alter$.altID), class(e$aatie$.srcID))
            expect_equal(class(e$alter$.altID), class(e$aatie$.tgtID))
            expect_equal(class(e$aatie$.tgtID), class(e$aatie$.srcID))
            
          })
