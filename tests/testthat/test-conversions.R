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

test_that("as_alters_df works",
          {
            expect_error({
              
              as_alters_df(egor32, include.ego.vars = TRUE)
              as_aaties_df(egor32)
              
            }, NA)
          })

test_that("as_tibble and other conversions work",
          {
            expect_error({
              
              as_tibble(egor32)
              activate(egor32, "aatie") %>%
                as_tibble(include.ego.vars = TRUE)
              
              activate(egor32, "alter") %>%
                as_tibble(include.ego.vars = TRUE)
              
              activate(egor32, "aatie") %>%
                as_tibble(include.alter.vars = TRUE)
              x <- activate(egor32, "aatie")
              
            }, NA)
          })

test_that("as_igraph.nested_egor works",
          {
            expect_error({
              
              e <- make_egor(5, 5)

              en <- as_nested_egor(e)
              as_igraph(en,
                        include.ego = T,
                        ego.attrs = c("sex", "age"))
              
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
              igraph::list.graph.attributes(igraph_list[[1]]) == c(".egoID",
                                                                   "income",
                                                                   "age")
            ))
            expect_true(is.atomic(igraph::graph.attributes(igraph_list[[1]])$.egoID))
          })

test_that("as_igraph works with egor32.",
          {
            res <- as_igraph(x = egor32)
            expect_equal(length(res), 32)
            expect_true(all(map_lgl(res, igraph::is.igraph)))
          })

test_that("as_alters_df works.",
          {
            expect_error(as_alters_df(egor32), NA)
            expect_error(as_alters_df(egor32, include.ego.vars = T), NA)
          })


test_that("as_aaties_df works.",
          {
            expect_error(as_aaties_df(egor32), NA)
            expect_error(as_aaties_df(object = egor32, include.alter.vars = T), NA)
            expect_error(as_aaties_df(object = egor32, include.ego.vars = T), NA)
            expect_error(as_aaties_df(
              object = egor32,
              include.ego.vars = T,
              include.alter.vars = TRUE
            ),
            NA)
          })


# test_that("as.network.egor() works with faux.mesa.high", {
#   library(ergm.ego)
#   data(faux.mesa.high)
#   fmh.ego <- as.egor(faux.mesa.high)
#   aaa <- as.network(x = fmh.ego)
# })