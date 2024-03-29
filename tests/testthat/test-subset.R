test_that("test subset.egor (doc example)", {
  # Generate a small sample dataset
  e <- make_egor(5, 4)
  
  # First three egos in the dataset
  e[1:3, ]
  
  # Using an external vector
  # (though normally, we would use e[.keep,] here)
  keep <- rep(c(TRUE, FALSE), length.out = nrow(e$ego))
  expect_error({subset(e, keep)}, NA)
})

test_that("test subset.egor works with all units", {
  e1 <- make_egor(5, 25)
  
  expect_error(e1[e1$ego$age.years > 35, ], NA)
  expect_error(subset(x = e1, subset = e1$ego$sex == "w", unit = "ego"), NA)
  expect_error(subset(x = e1, subset = sex == "w", unit = "ego"), NA)
  expect_error(subset(x = e1, subset = e1$alter$sex == "m", unit = "alter"), NA)
  expect_error(subset(e1, e1$aatie$weight > 0.5, unit = "aatie"), NA)
  
})


test_that("subset.egor() works as specified in docs", {
  data(egor32)
  
  expect_error({
    # Filter egos
    subset(x = egor32, subset = egor32$ego$variables$sex == "m", unit="ego")
    subset(x = egor32, sex == "m")
    
    # Filter alters
    subset(x = egor32, sex == "m", unit = "alter")
    
    # Filter aaties
    subset(x = egor32, weight != 0, unit = "aatie")
    
    # Filter egos by alter variables (keep only egos that have more than 13 alters)
    subset(x = egor32, ego$country == "Poland", unit = "ego")
    
    # Filter alters by ego variables (keep only alters that have egos from Poland)
    subset(x = egor32, nrow(alter) > 13, unit = "alter")
    
    # Filter edges by alter variables (keep only edges between alters where `sex == "m"`)
    subset(x = egor32, all(alter$sex == "m"), unit = "aatie")
    
  }, NA)
  test1 <- subset(egor32, sex == "m", unit="ego")
  expect_equal(as.character(unique(test1$ego$variables$sex)), "m")
  
  test2 <- subset(egor32, country == "Australia")
  expect_equal(test2$ego$variables$country[1], "Australia")
  
  test3 <- subset(egor32, ego$sex == "m", unit = "alter")
  test3a <- test3 %>% 
    filter(.egoID %in% unique(test3$alter$.egoID)) %>% 
    pull(sex) %>% as.character()
  expect_equal(unique(test3a), "m")  
  
})

test_that("subset() works when no aaties are specified and ID vars are character #74", {
  e1 <-
    egor(alters = data.frame(
      egoID = c(1, 2, 3),
      alterID = rep(1, 3),
      something = c(1, 2, 3)
    ))
  e2 <-
    egor(
      alters = data.frame(
        egoID = c(1, 2, 3),
        alterID = rep(1, 3),
        something = c(1, 2, 3)
      ),
      aaties = data.frame(
        egoID = 1:3,
        Source = rep(1, 3),
        Target = rep(1, 3)
      )
    )
  expect_error(subset(x = e1, something == 1, unit = "alter"), NA)
  expect_error(e1 %>% 
    activate(alter) %>% 
    filter(something == 1), NA)
  expect_error(subset(e2, something == 1, unit = "alter"), NA)
  expect_error(e2 %>% 
    activate(alter) %>% 
    filter(something == 1), NA)
})
