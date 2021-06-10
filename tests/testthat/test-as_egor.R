test_that("as.egor.list() works for igraph objects.",{
  
  # Create a list of igraph objects that represent ego networks.
  # no egos
  ig_l1 <-
    egor32 %>% 
    as_igraph(
      ego.attrs = c("sex", "age", "age.years", "country", "income"),
      graph.attrs = c(".egoID", "age")
    )
  
  expect_error(as.egor(x = ig_l1), NA)
  
  # with egos
  ig_l2 <-
    egor32 %>%
    as_igraph(
      include.ego = TRUE,
      ego.attrs = c("sex", "age", "age.years", "country", "income"),
      graph.attrs = c(".egoID", "age")
    )
  
  expect_error(as.egor(x = ig_l2, ego_name = "ego"), NA)
  expect_error(as.egor(x = ig_l2,
          ego_name = rep("ego", 32)), NA)
})


test_that("as.egor.list() works for network objects.",{
  
  net_l1 <-
    as_network(x = egor32,
      ego.attrs = c("sex", "age", "age.years", "country", "income"),
      graph.attrs = c(".egoID", "age")
    )
  
  expect_error(as.egor(x = net_l1), NA)
  
  # with egos
  net_l2 <-
    as_network(
      x = egor32,
      include.ego = TRUE,
      ego.attrs = c("sex", "age", "age.years", "country", "income"),
      graph.attrs = c(".egoID", "age")
    )
  
  expect_error(as.egor(x = net_l2, ego_name = "ego"), NA)
  expect_error(as.egor(x = net_l2,
                       ego_name = rep("ego", 32)), NA)
})



