library(testthat)

data(egor32)

# Mutate
res <- mutate(egor32, b = sex)
expect_is(res, "egor")

res <- transmute(egor32, b = sex)
expect_is(res, "egor")

# Select
res <- select(egor32_g, "sex")
expect_is(res, "egor")

res <- rename(egor32, pim = "sex")
expect_is(res, "egor")

# Filter
res <- filter(egor32, sex == "w", age == "18 - 25") # This will currently not subset the svydesign object corretly...
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

