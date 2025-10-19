# todo
# as_igraph() returns empty list for
# problem: if there are no aaties for an ego, that ego is not created as a network
# is that the same for alters?

make_egor(1, 2, netsize_fixed = TRUE) %>%
  as_igraph()

x <- make_egor(1, 2, netsize_fixed = TRUE)

# igraph ------------------------------------------------------------------

test_that("as_igraph() works correctly when one or more egos have no aaties", {
  
  
  alters <- tibble(egoID = gl(4,4), 
                   alterID = rep(1:4, 4),
                   fav_color = sample(c("red", "green", "blue"), 16, replace = TRUE))
  aaties <- tibble(egoID = sample(1:3, 32, replace = TRUE),
                   Source = sample(1:4, 32, replace = TRUE),
                   Target = sample(1:4, 32, replace = TRUE))
  e <- egor(alters, aaties = aaties)
  
  expect_error(as_igraph(e), NA)
  })

test_that("as_igraph() does not error when egor object has ego design", {
  expect_error(as_igraph(x = egor32), NA)
})

test_that("as_igraph() includes `graph.attrs`", {
  g1 <- as_igraph(egor32, graph.attrs = c(".egoID"))
  
  expect_contains(igraph::graph_attr_names(g1[[1]]), c(".egoID"))
  
  g2 <- as_igraph(egor32, graph.attrs = c(".egoID", "age"))
  
  expect_contains(igraph::graph_attr_names(g2[[1]]), c(".egoID", "age"))
  
})

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
  expect_equal(egos_after_as_igraph$age, e1$ego$age)

})

test_that("as_igraph works with egor32.",
          {
            res <- as_igraph(x = egor32)
            expect_equal(length(res), 32)
            expect_true(all(purrr::map_lgl(res, igraph::is.igraph)))
          })

test_that("as_igraph works.",
          {
            expect_error(as_igraph(x = egor32), NA, label = "default arguments")
            
            egor32$alter <- egor32$alter %>%
              mutate(weight = sample((1:3) / 3, nrow(.), replace = TRUE))
            
            expect_error(igraph_list <- as_igraph(
              egor32,
              include.ego = T,
              ego.attrs = c("sex", "age"),
              ego.alter.weights = "weight"
            ),
            NA)
            
            expect_true("ego" %in% igraph::V(igraph_list[[1]])$name)
            expect_false(any(is.na(igraph::V(igraph_list[[1]])$sex)))
            
          })

test_that("as_igraph works with several graph attributes",
          {
            expect_error(igraph_list <-
                           as_igraph(egor32,
                                     graph.attrs = c(".egoID", 
                                                     "income", 
                                                     "age")),
                         NA)
            
            expect_true(all(
              igraph::graph_attr_names(igraph_list[[1]]) == c(".egoID",
                                                              "income",
                                                              "age")
            ))
            expect_true(is.atomic(igraph::graph.attributes(igraph_list[[1]])$.egoID))
          })


# network -----------------------------------------------------------------

test_that("as_network() does not error when egor object has ego design", {
  expect_error(as_network(egor32), NA)
})

test_that("as_network works",
          {
            expect_error({
              
              as_network(egor32)
              as_network(x = egor32, include.ego = TRUE)
              as_network(x = egor32,
                         ego.attrs = "sex",
                         include.ego = TRUE)
            }, NA)
            
          })

test_that("as_network works.",
          {
            expect_error(network::as.network(egor32), NA, label = "default arguments")
            
            egor32$alter <- egor32$alter %>%
              mutate(weight = sample((1:3) / 3, nrow(.), replace = TRUE))
            
            expect_error(
              as_network(
                x = egor32,
                include.ego = TRUE,
                ego.attrs = c("sex", "age"),
                ego.alter.weights = "weight"
              ),
              NA,
              label = "include.ego/ego.attrs/ego.alter.weights"
            )
          })

test_that("as_network works with graph.attrs",
          {
            
            expect_error(res <- as_network(x = egor32, graph.attrs = c(".egoID", "income")
            ), NA)
            
            expect_true(all(
              c("income" , ".egoID") %in% 
                network::list.network.attributes(res[[1]])
            ))
            
          })

# test_that("as.network.egor() works with faux.mesa.high", {
#   library(ergm.ego)
#   data(faux.mesa.high)
#   fmh.ego <- as.egor(faux.mesa.high)
#   aaa <- as.network(x = fmh.ego)
# })