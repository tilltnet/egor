context("test_dplyr_methods.R")

test_that("methods for dplyr are working",
          {
            expect_error({
              e <- make_egor(5, 50)
              #e
              trim_aaties(e)
              trim_alters(e)
              
              mutate(e, sex = 1)
              
              
              e %>%
                activate("aatie") %>%
                mutate(weight2 = 2 + weight) %>%
                activate("alter") %>%
                mutate(age.years = age.years ^ 3)
              
              mutate(e, x = 1)
              transmute(e, .egoID = age.years / 2)
              
              
              
              select(e, .egoID, sex)
              group_by(e, country)
              
              count(as_tibble(e), age)
              tally(as_tibble(e), income)
              # add_count(e, income)
              # add_tally(e, income)
              
              top_n(e, 2, income)
              egor::activate(e, alter) %>%
                top_n(2, income)
              
              
              result <- transmute(e[[attr(e, "active")]], x = age.years / 2)
              egor:::bind_IDs_if_missing(.data = e, result = result)
              
              filter(e, sex == "w")$alter
              
              rename(e, p = sex)
              egor::activate(e, "alter") %>%
                filter(sex == "w")
              
              slice(e, 1)
              
              activate(e, alter) %>%
                slice(1, 5, 6)
              
              activate(e, aatie) %>%
                slice(1:2)
              
              e %>%
                activate("alter") %>%
                group_by(.egoID) %>%
                summarise(average_age = mean(age.years))
              
              arrange(e, age.years)
              arrange(e, desc(age.years))
              
              full_join(e, tibble(.egoID = 1, new_data = "asdwd"))
              
              
              select_all(e, toupper)
              rename_all(e, toupper)
              
              egor::activate(e, alter) %>%
                select_at(vars(contains("x")), toupper)
              rename_at(e, .vars = vars(contains("x")), toupper)
              mutate_at(e, vars(sex, country), toupper)
              
              
              #select_if(e, is.factor, toupper)
              #rename_if(e, function(x) is.factor(x), toupper)
              #mutate_if(e, is.double, function(x) x^2)
              
              tbl_vars(e)
              group_vars(e)
            }, NA)
          })

test_that(
  "pull works",
  {
    e <- make_egor(10, 10)
    expect_error(pull(e, sex), NA)
  }
)

# test_that(
#   "dplyr::across() works with egor object",{
#   e <- make_egor(10, 10)
#
#   e %>%
#     summarise(across(is.double, mean))
#
#   e %>%
#     mutate(across(is.character, toupper))
#
#   e %>%
#     mutate(across(starts_with("age"), as.character))
#
#   e %>%
#     activate(alter) %>%
#     group_by(.egoID) %>%
#     summarise(across(is.numeric, mean, .names = "chr_{col}"))
# })

test_that("rowwise.egor() works", {
  e <- make_egor(10, 10)
  res <- rowwise(data = e) %>%
    mutate(a = list(c(age.years, country)))
  expect_equal(length(res$ego$a[[1]]), 2)
})

# test_that("relocate.egor() works", {
#   e <- make_egor(10, 10)
#   e %>%
#     relocate(age, income)
# })
#
# test_that("rename_with() works", {
#   e <- make_egor(10, 10)
#   e %>%
#     rename_with(toupper)
# })

test_that("methods for dplyr verbs keep egor class/attributes",
          {
            e <- make_egor(3, 15)
            # Mutate
            trim_aaties(e)
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
            res <- filter(e, sex == "w", age == "18 - 25")
            res <- filter(e, sex == "w" & age == "18 - 25")
            expect_is(res, "egor")
            
            # Summarise in dplyr 0.8.0 doesnt preserve the egor class...
            # ... is that necessary?
            #res <- summarise(e, sum(as.numeric(age)))
            #expect_is(res, "egor")
            
            # Group By
            eg <- group_by(e, sex)
            expect_is(eg, "egor")
            expect_is(eg$ego, "grouped_df")
            
            res <- summarise(eg, s = sum(as.numeric(age)))
            expect_true(NCOL(res) == 2)
            
            #library(srvyr) # For unweighted.
            ego_design(e) <- list(weights = 1)
            eg <- group_by(e, sex)
            expect_is(eg, "egor")
            expect_is(eg$ego, "grouped_svy")
            #expect_warning(summarise(eg, s = unweighted(sum(as.numeric(age)))), NA)
            trace(tbl_df)
            res <-
              summarise(eg, s = srvyr::unweighted(sum(as.numeric(age))))
            untrace(tbl_df)
            expect_true(NCOL(res) == 2)
          })

test_that("aaties are trimmed correctly when filtering .alts",
          {
            e <- make_egor(12, 15)
            e2 <- filter(e, sex == "w")
            expect_lt(
              nrow(e2$aatie),
              nrow(e$aatie),
              "Check that deletion of alters leads to deletions in aaties."
            )
            
            e3 <- activate(e, "alter") %>%
              filter(sex == "w")
            expect_lt(
              nrow(e3$aatie),
              nrow(e$aatie),
              "Check that mutating alters leads to deletions in aaties."
            )
          })

test_that("quasi quotation works with activate",
          {
            e <- make_egor(12, 15)
            expect_error(activate(e, alter), NA)
            expect_error(activate(e, "aatie"), NA)
          })

test_that("group_modify.egor and work",
          {
            e <- make_egor(12, 15)
            
            a <- e %>%
              group_by(country)
            class(a)
            a$ego
            
            expect_error(e %>%
                           group_by(country) %>%
                           group_modify(function(x, ...)
                             x %>%
                               mutate(income2 = income - mean(income))),
                         NA)
            
            
          })

test_that("append_rows, append_cols work",
          {
            expect_error({
              e <- make_egor(12, 15)
              
              # Adding a column to the ego level
              
              additional_ego_columns <-
                tibble(x = sample(1:3, 12, replace = TRUE))
              
              append_cols(e, additional_ego_columns)
              
              # Adding rows to the ego and alter level
              
              additional_ego_rows <-
                list(
                  .egoID = 13,
                  sex = "w",
                  age = factor("56 - 65"),
                  age.years = 60,
                  country = "Australia"
                ) %>%
                as_tibble()
              
              additional_alter_rows <-
                list(
                  .altID = 1:5,
                  .egoID = rep(13, 5),
                  sex = sample(c("f", "m"), 5, replace = TRUE)
                ) %>%
                as_tibble()
              
              append_rows(e, additional_ego_rows) %>%
                activate(alter) %>%
                append_rows(additional_alter_rows)
            }, NA)
          })
