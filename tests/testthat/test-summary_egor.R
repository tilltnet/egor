test_that("summary.egor counts egos with no alters as 0", {
  
  #' Here is a test case to replicate #94 - `summary.egor()`: egos with zero alters not counted for summary stats.
  
  # library(egor)
  
  #' `egor32` has 32 networks of size 12.
  egor32 |>
    activate(alter) |>
    summarise(n(), .by = ".egoID")
  
  #' When we delete all alters of one ego the network size should be `31 * 12 / 32 = 11.625`, but in `egor` version 1.24.2 it is calculated as `31 * 12 / 31 = 12`.
  egor32 |>
    activate(alter) |>
    filter(.egoID != 2)
  
  egor32b <-
    egor32 |>
    activate(alter) |>
    filter(.egoID != 2)
  
  res1 <- 
    summary(egor32)
  
  res2 <- 
    summary(egor32b)
  
  expect_equal(res1$value[res1$stat == "min. Netsize"], 12)
  expect_equal(res1$value[res1$stat == "Average Netsize"], 12)
  
  expect_equal(res2$value[res1$stat == "min. Netsize"], 0)
  expect_equal(res2$value[res1$stat == "Average Netsize"], 11.625)
  
})

test_that("summary.egor() uses weights to calculate average network size", {

  e1 <- egor(alters =
         tibble(egoID = c(1, 1, 2, 2, 2), alterID = c(1:2, 1:3)),
       egos = 
         tibble(egoID = 1:2, weight = c(0.5, 1)))
  
  ego_design(e1) <- alist(probs = weight)
  
  res1 <- summary(e1)
  
  expect_equal(res1$value[res1$stat == "Average Netsize"], 2 + 1 / 3)
  
  e2 <- make_egor(5, 5)
  
  e2 <-
    e2 |>
    mutate(w = sample(seq(0.1, 0.9, 0.1), 5))
  
  ego_design(e2) <- alist(probs = w)
  
  res2 <- summary.egor(e2)
  
  netsize_no_weights <- e2 |> 
    activate(alter) |> 
    summarise(netsize = n(), .by = ".egoID") |> 
    pull(netsize) |> 
    mean()
  
  netsize_with_weights <- 
    res2$value[res2$stat == "Average Netsize"]
  
  expect_false(netsize_no_weights == netsize_with_weights)
})
