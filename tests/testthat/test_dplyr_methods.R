context("test_dplyr_methods.R")

library(testthat)
library(dplyr)


test_that(
  "methods for dplyr verbs keep egor class/attributes",
  {
    e <- make_egor(3, 5)
    # Mutate
    res <- mutate(e, b = sex)
    expect_is(res, "egor")
    res <- transmute(e, b = sex)
    expect_is(res, "egor")
    
    # Select
    res <- select(e, "sex")
    expect_is(res, "egor")
    
    res <- rename(e, pim = "sex")
    expect_is(res, "egor")
    
    # Filter
    res <- filter(e, sex == "w", age == "18 - 25") # This will currently not subset the svydesign object correctly...
    res <- filter(e, sex == "w" & age == "18 - 25") # ...while this will.
    expect_is(res, "egor")
    
    # Summarise & Group By
    res <- summarise(e, sum(as.numeric(age)))
    expect_is(res, "egor")
    
    res <- group_by(e, sex)
    expect_is(res, "egor")
    expect_is(res, "grouped_df")
    
    res <- summarise(res, sum(as.numeric(age)))
    expect_true(NCOL(res) == 2)
  }
)

test_that(
  "aaties are trimmed correctly when mutating .alts",
  {
    e <- make_egor(2, 5)
    e$.alts <- purrr::map(e$.alts, ~{
      filter(., sex == "p")
    })
    e2 <- trim_aaties(e)
    expect_lt(sum(purrr::map_dbl(e2$.aaties, nrow)), 
              sum(purrr::map_dbl(e$.aaties, nrow)),
              "Check that deletion of alters leads to deletions in aaties.")
  
    e3 <- mutate(e, .alts = purrr::map(.alts, ~{filter(., sex=="p")}))
    expect_lt(sum(purrr::map_dbl(e3$.aaties, nrow)), 
              sum(purrr::map_dbl(e$.aaties, nrow)),
              "Check that mutating alters leads to deletions in aaties.")
  }
)


       