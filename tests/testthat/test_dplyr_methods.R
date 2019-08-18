context("test_dplyr_methods.R")


test_that(
  "methods for dplyr are working",
  {
    expect_error({
      e <- make_egor(5,50)
      e
      trim_aaties(e)
      trim_alters(e)
      mutate(e, sex = 1)
      
      e %>% 
        activate("aatie") %>% 
        mutate(weight2 = 2 + weight) %>% 
        activate("alter") %>% 
        mutate(age.years = age.years^3)
      
      mutate(e, x = 1)
      transmute.egor(e, x = age.years/2)
      count(e, age)
      select.egor(e, sex)
      filter(e, sex == "w")$alter
      
      activate(e, "alter") %>% 
        filter(sex == "w")
      
      e %>% 
        activate("alter") %>% 
        group_by(.egoID) %>% 
        summarise(average_age = mean(age.years))
      
      arrange(e, age.years)
      arrange(e, desc(age.years))
      
      full_join(e, tibble(.egoID = factor(1), new_data = "asdwd"), NA)
    })
  }
)



test_that(
  "methods for dplyr verbs keep egor class/attributes",
  {
    e <- make_egor(3, 5)
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
    res <- group_by(e, sex)
    expect_is(res, "egor")
    expect_is(res$ego, "grouped_df")
    
    res <- summarise(res, sum(as.numeric(age)))
    expect_true(NCOL(res) == 2)
  }
)

test_that(
  "aaties are trimmed correctly when filtering .alts",
  {
    e <- make_egor(12, 15)
    e2 <- filter(e, sex == "w")
    expect_lt(nrow(e2$aatie), 
              nrow(e$aatie),
              "Check that deletion of alters leads to deletions in aaties.")
  
    e3 <- activate(e, "alter") %>% 
              filter(sex=="w")
    expect_lt(nrow(e3$aatie), 
              nrow(e$aatie),
              "Check that mutating alters leads to deletions in aaties.")
  }
)


       