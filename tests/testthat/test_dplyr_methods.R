context("test_dplyr_methods.R")

library(testthat)
library(dplyr)
data(egor32)

# Mutate
res <- mutate(egor32, b = sex)
expect_is(res, "egor")

res <- transmute(egor32, b = sex)
expect_is(res, "egor")

# Select
res <- select(egor32, "sex")
expect_is(res, "egor")

res <- rename(egor32, pim = "sex")
expect_is(res, "egor")

# Filter
res <- filter(egor32, sex == "w", age == "18 - 25") # This will currently not subset the svydesign object correctly...
res <- filter(egor32, sex == "w" & age == "18 - 25") # ...while this will.
expect_is(res, "egor")

# Summarise & Group By
res <- summarise(egor32, sum(as.numeric(age)))
expect_is(res, "egor")

res <- group_by(egor32, sex)
expect_is(res, "egor")
expect_is(res, "grouped_df")

res <- summarise(res, sum(as.numeric(age)))
expect_true(NCOL(res) == 2)

e2 <- egor32
e2$.alts <- map(e2$.alts, ~{
    filter(., sex == "w")
  })
e2 <- trim_aaties(e2)
expect_lt(sum(map_dbl(e2$.aaties, nrow)), 
          sum(map_dbl(egor32$.aaties, nrow)),
          "Check that deletion of alters leads to deletions in aaties.")



e3 <- mutate(egor32, .alts = map(.alts, ~{filter(., sex=="w")}))
expect_lt(sum(map_dbl(e3$.aaties, nrow)), 
          sum(map_dbl(egor32$.aaties, nrow)),
          "Check that deletion of alters leads to deletions in aaties.")

       