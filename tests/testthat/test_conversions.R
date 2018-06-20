context("test_conversions.R")

test_that(
  "as_network works.",
  {
    e <- make_egor(3, 22)
    expect_error(network::as.network(e), NA, label = "default arguments")
    
    e$.alts <- e$.alts %>%
      purrr::map(
        ~{
          .$weight <- sample((1:3)/3, nrow(.), replace = TRUE)
          .
        }
      )
    expect_error(
      as_network(x = e,
                 include.ego = T, 
                 ego.attrs = c("sex", "age"),
                 ego.alter.weights = "weight"),
      NA, 
      label = "include.ego/ego.attrs/ego.alter.weights")
  }
)

test_that(
  "as_igraph works.",
  {
    e <- make_egor(3, 22)
    expect_error(as_igraph(e), NA, label = "default arguments")
    
    e$.alts <- e$.alts %>%
      purrr::map(
        ~{
          .$weight <- sample((1:3)/3, nrow(.), replace = TRUE)
          .
        }
      )
    expect_error(
      as_igraph(e,
                 include.ego = T, 
                 ego.attrs = c("sex", "age"),
                 ego.alter.weights = "weight"),
      NA, 
      label = "include.ego/ego.attrs/ego.alter.weights")
  }
)

test_that(
  "as_alts_df works.",
  {
    e <- make_egor(3, 22)
    expect_error(as_alts_df(e), NA)
    expect_error(as_alts_df(e, "ID"), NA)
    expect_error(as_alts_df(e, include.ego.vars = T), NA)
  }
)


test_that(
  "as_aaties_df works.",
  {
    e <- make_egor(3, 22)
    expect_error(as_aaties_df(egor32), NA)
    expect_error(as_aaties_df(egor32, egoID = "ID"), NA)
    expect_error(as_aaties_df(object = egor32, include.alt.vars = T), NA)
  }
)

