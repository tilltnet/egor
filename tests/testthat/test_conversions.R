context("test_conversions.R")

test_that("as_tibble and other conversions work",
          {
            expect_error({
              e <- make_egor(3, 22)
              
              as_network(e)
              as_network(x = e, include.ego = TRUE)
              as_network(x = e,
                         ego.attrs = "sex",
                         include.ego = TRUE)
              
              as_tibble(e)
              activate(e, "aatie") %>%
                as_tibble(include.ego.vars = TRUE)
              
              activate(e, "alter") %>%
                as_tibble(include.ego.vars = TRUE)
              
              activate(e, "aatie") %>%
                as_tibble(include.alter.vars = TRUE)
              x <- activate(e, "aatie")
              as_alters_df(e, include.ego.vars = TRUE)
              as_aaties_df(e)
            }, NA)
          })

test_that("as_igraph.nested_egor works",
          {
            expect_error({
            e <- make_egor(3, 22)
            
            en <- as_nested_egor(e)
            as_igraph(en,
                      include.ego = T,
                      ego.attrs = c("sex", "age"))
            }, NA)
          })

test_that("as_network works.",
          {
            e <- make_egor(3, 22)
            expect_error(network::as.network(e), NA, label = "default arguments")
            
            e$alter <- e$alter %>%
              mutate(weight = sample((1:3) / 3, nrow(.), replace = TRUE))
            
            expect_error(
              as_network(
                x = e,
                include.ego = TRUE,
                ego.attrs = c("sex", "age"),
                ego.alter.weights = "weight"
              ),
              NA,
              label = "include.ego/ego.attrs/ego.alter.weights"
            )
          })

test_that("as_igraph works.",
          {
            e <- make_egor(3, 22)
            expect_error(as_igraph(e), NA, label = "default arguments")
            
            e$alter <- e$alter %>%
              mutate(weight = sample((1:3) / 3, nrow(.), replace = TRUE))
            
            expect_error(as_igraph(
              e,
              include.ego = T,
              ego.attrs = c("sex", "age"),
              ego.alter.weights = "weight"
            ),
            NA,
            label = "include.ego/ego.attrs/ego.alter.weights")
          })

test_that("as_alters_df works.",
          {
            e <- make_egor(3, 22)
            expect_error(as_alters_df(e), NA)
            expect_error(as_alters_df(e, include.ego.vars = T), NA)
          })


test_that("as_aaties_df works.",
          {
            e <- make_egor(3, 22)
            expect_error(as_aaties_df(e), NA)
            expect_error(as_aaties_df(object = e, include.alt.vars = T), NA)
          })
