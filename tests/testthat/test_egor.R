context("test_egor.R")

# only alters

test_that(
  "egor() works when only alters are provided",
  {
    alters <- tibble(egoID = gl(4,4), 
                     alterID = rep(1:4, 4),
                     fav_color = sample(c("red", "green", "blue"), 16, replace = TRUE))
    expect_error(egor(alters), NA)
  }
)

# alters+egos

test_that(
  "egor() works when only alters and egos are provided",
  {
    alters <- tibble(egoID = gl(4,4), 
                     alterID = rep(1:4, 4),
                     fav_color = sample(c("red", "green", "blue"), 16, replace = TRUE))
    egos <- tibble(egoID = 1:5)
    expect_error(egor(alters, egos), NA)
  }
)



# alters+aaties

test_that(
  "egor() works when only alters and aaties are provided",
  {
    alters <- tibble(egoID = gl(4,4), 
                     alterID = rep(1:4, 4),
                     fav_color = sample(c("red", "green", "blue"), 16, replace = TRUE))
    aaties <- tibble(egoID = sample(1:4, 32, replace = TRUE),
                     Source = sample(1:4, 32, replace = TRUE),
                     Target = sample(1:4, 32, replace = TRUE))
    expect_error(egor(alters, aaties = aaties), NA)
  }
)

# alters+egos+aaties

test_that(
  "egor() works when all three levels are provided",
  {
    alters <- tibble(egoID = gl(4,4), 
                     alterID = rep(1:4, 4),
                     fav_color = sample(c("red", "green", "blue"), 16, replace = TRUE))
    aaties <- tibble(egoID = sample(1:5, 32, replace = TRUE),
                     Source = sample(1:4, 32, replace = TRUE),
                     Target = sample(1:4, 32, replace = TRUE))
    egos <- tibble(egoID = 1:5)
    expect_error(egor(alters, egos, aaties = aaties), NA)
  }
)

test_that(
  "egor object's components can be extracted and rebuild to an egor object.",
  {
    e <- make_egor(32, 20)
    test_that("e is egor object", 
              expect_true((class(e) == "egor")[1]))
    
    egos <- e$ego
    alters <- e$alter
    aaties <- e$aatie
    
    names(egos)[1] <- "egoID"
    names(alters)[1] <- "alterID"
    names(alters)[2] <- "egoID"
    names(aaties)[1:3] <- c("egoID", "Source", "Target")
    
    expect_error(e1 <- egor(alters, egos, aaties), NA)
    expect_true((class(e1)[1] == "egor"))
  }
)

test_that(
  "summary.egor works",
  {
    expect_error(summary(make_egor(3,20)), NA)
  }
)

test_that(
  "egor() works with missing alters/ aaties / egos.",
  {
    alters <- tibble(egoID = gl(3,4), 
                     alterID = rep(1:3, 4),
                     fav_color = sample(c("red", "green", "blue"), 12, replace = TRUE))
    aaties <- tibble(egoID = sample(1:3, 32, replace = TRUE),
                     Source = sample(1:4, 32, replace = TRUE),
                     Target = sample(1:4, 32, replace = TRUE))
    egos <- tibble(egoID = 1:5)
    
    
    expect_error(egor(alters,
                      egos, 
                      aaties), NA)
    expect_warning(egor(alters,
                        egos, 
                        aaties), NA)
    
    expect_error(summary(egor(alters, egos, aaties)), NA)
  }
)


test_that(
  "egor(): Non-Unique egoID in ego data should raise a warning)",
  {
    alters <- tibble(egoID = gl(3,4), 
                     alterID = rep(1:3, 4),
                     fav_color = sample(c("red", "green", "blue"), 12, replace = TRUE))
    aaties <- tibble(egoID = sample(1:3, 32, replace = TRUE),
                     Source = sample(1:4, 32, replace = TRUE),
                     Target = sample(1:4, 32, replace = TRUE))
    egos <- tibble(egoID = c(1, 1:4))
    
    
    expect_warning(egor(alters,
                      egos, 
                      aaties))
    
    expect_warning(summary(egor(alters, egos, aaties)))
  }
)


test_that(
  "egor(): Alters without egos should raise a warning)",
  {
    alters <- tibble(egoID = gl(3,4), 
                     alterID = rep(1:3, 4),
                     fav_color = sample(c("red", "green", "blue"), 12, replace = TRUE))
    aaties <- tibble(egoID = sample(1:3, 32, replace = TRUE),
                     Source = sample(1:4, 32, replace = TRUE),
                     Target = sample(1:4, 32, replace = TRUE))
    egos <- tibble(egoID = 2:4)
    
    
    expect_warning(egor(alters,
                      egos, 
                      aaties))
    
    expect_warning(summary(egor(alters, egos, aaties)))
  }
)

# What of Input is dataframes? (NOT tibbles)


test_that(
  "egor(): can process dataframes that are not tibbles.)",
  {
    alters <- tibble(egoID = gl(4,4), 
                     alterID = rep(1:4, 4),
                     fav_color = sample(c("red", "green", "blue"), 16, replace = TRUE))
    aaties <- tibble(egoID = sample(1:3, 32, replace = TRUE),
                     Source = sample(1:4, 32, replace = TRUE),
                     Target = sample(1:4, 32, replace = TRUE))
    egos <- tibble(egoID = 1:4)
    
    alters_df <- data.frame(egoID = gl(4,4), 
                     alterID = rep(1:4, 4),
                     fav_color = sample(c("red", "green", "blue"), 16, replace = TRUE))
    aaties_df <- data.frame(egoID = sample(1:3, 32, replace = TRUE),
                     Source = sample(1:4, 32, replace = TRUE),
                     Target = sample(1:4, 32, replace = TRUE))
    egos_df <- data.frame(egoID = 1:4)
    
    
    expect_error(egor(alters_df,
                      egos_df,
                      aaties_df), NA)
    
    expect_error(egor(alters_df,
                      egos,
                      aaties), NA)
    
    expect_error(egor(alters,
                      egos_df,
                      aaties), NA)
    
    expect_error(egor(alters,
                      egos,
                      aaties_df), NA)
    
    expect_warning(summary(egor(alters_df, egos_df, aaties_df)), NA)
  }
)

