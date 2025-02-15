test_that("as_igraph correctly transfers ego attributes", {
  # library(egor)
  # library(tidyverse)
  # library(igraph)
  
  e1 <- egor(
    alters = alters32,
    egos = egos32,
    aaties = aaties32,
    ID.vars = list(
      ego = ".EGOID",
      alter = ".ALTID",
      source = ".SRCID",
      target = ".TGTID"
    )
  ) |> 
    activate(alter) |> 
    mutate(e_a_weight = sample(1:384/384, 384))

  g_l <- 
    e1 %>%
    as_igraph(include.ego = TRUE,
              ego.attrs = c("sex", "country", "age"))
  
  egos_after_as_igraph <-
    purrr::map(g_l, igraph::as_data_frame, "vertices") |>
    purrr::map(filter, name == "ego") |>
    purrr::list_rbind(names_to = "egoID")

  expect_equal(egos_after_as_igraph$country, e1$ego$country)
  expect_equal(egos_after_as_igraph$sex, e1$ego$sex)
  expect_equal(egos_after_as_igraph$age, as.character(e1$ego$age))

})
