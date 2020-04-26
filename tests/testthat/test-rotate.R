test_that("rotate_xy() works", {
  expect_error(
    make_egor(1, 4, netsize_fixed = TRUE) %>%
      as_igraph() %>%
      .[[1]] %>%
      igraph::layout_in_circle() %>%
      rotate_xy(1),
    NA
  )
})

test_that("rotate_xy() works with a 2x2 matrix", {
  expect_error(
    make_egor(1, 2, netsize_fixed = TRUE) %>%
      as_igraph() %>%
      .[[1]] %>%
      igraph::layout_in_circle() %>%
      rotate_xy(1),
    NA
  )
})

test_that("rotate_to_equilibrium() works", {
  expect_error(
    make_egor(1, 4, netsize_fixed = TRUE) %>%
      as_igraph() %>%
      .[[1]] %>%
      igraph::layout_in_circle() %>%
      rotate_to_equilibrium(),
    NA
  )
})

test_that("rotate_to_equilibrium() works with a 2x2 matrix", {
  expect_error(
    make_egor(1, 2, netsize_fixed = TRUE) %>%
      as_igraph() %>%
      .[[1]] %>%
      igraph::layout_in_circle() %>%
      rotate_to_equilibrium(),
    NA
  )
  
  expect_error(
    make_egor(1, 2, netsize_fixed = TRUE) %>%
      as_igraph() %>%
      .[[1]] %>%
      igraph::layout_as_star() %>%
      rotate_to_equilibrium(),
    NA
  )
})

test_that("rotate_to_equilibrium() works with a 3x2 matrix", {
  expect_error(
    make_egor(1, 3, netsize_fixed = TRUE) %>%
      as_igraph() %>%
      .[[1]] %>%
      igraph::layout_in_circle() %>%
      rotate_to_equilibrium(),
    NA
  )
  
  expect_error(
    make_egor(1, 3, netsize_fixed = TRUE) %>%
      as_igraph() %>%
      .[[1]] %>%
      igraph::layout_as_star() %>%
      rotate_to_equilibrium(),
    NA
  )
})

# test_that("rotate_to_equilibrium() 2 coords with same y", {
#   lay_ <- matrix(c(-1,0,1,0,0,-1,0,1), 4, 2)
#   grph <- make_egor(1, 4, netsize_fixed = TRUE) %>%
#     as_igraph() %>% 
#     .[[1]]
#   
#   #plot(grph, layout = lay_)
#   #plot(grph, layout = rotate_to_equilibrium(xy_mat = lay_))
# })

if (FALSE) {
  graph_2 <-
    make_egor(1, 2, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  graph_3 <-
    make_egor(1, 3, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  graph_4 <-
    make_egor(1, 4, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  graph_5 <-
    make_egor(1, 5, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  graph_6 <-
    make_egor(1, 6, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  graph_7 <-
    make_egor(1, 7, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  graph_8 <-
    make_egor(1, 8, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  graph_9 <-
    make_egor(1, 9, netsize_fixed = TRUE) %>% as_igraph() %>% .[[1]]
  
  test_plot_with_rotation <-
    function(graph) {
      par(mfrow = c(1, 2))
      plot(graph, layout = layout_in_circle(graph))
      plot(graph, layout = rotate_to_equilibrium(layout_in_circle(graph)))
      par(mfrow = c(1, 1))
    }
  
  test_plot_with_rotation2 <-
    function(graph) {
      par(mfrow = c(1, 2))
      plot(graph, layout = layout_as_star(graph, 1))
      plot(graph, layout = rotate_to_equilibrium(layout_as_star(graph, 1)))
      par(mfrow = c(1, 1))
    }
  
  test_plot_with_rotation(graph = graph_2)
  test_plot_with_rotation2(graph_2)
  test_plot_with_rotation(graph_3)
  test_plot_with_rotation2(graph_3)
  test_plot_with_rotation(graph = graph_4)
  test_plot_with_rotation2(graph_4)
  test_plot_with_rotation(graph_5)
  test_plot_with_rotation2(graph_5)
  test_plot_with_rotation(graph_6)
  test_plot_with_rotation2(graph_6)
  test_plot_with_rotation(graph_7)
  test_plot_with_rotation2(graph_7)
  test_plot_with_rotation(graph_8)
  test_plot_with_rotation2(graph_8)
  test_plot_with_rotation(graph_9)
  test_plot_with_rotation2(graph_9)
}
