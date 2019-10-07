context("test_clustered_graphs.R")

test_that(
  "Methods are working.",
  {
  # Create test data
  mpf <- make_egor(5, 50)

  expect_error(clustered_graphs(mpf, "country"), NA)
  #expect_error(clustered_graphs(mpf$alter, mpf$aatie, "country"), NA, egoID = ".egoID")
  }
)

test_that(
  "Methods are working with partially missing data.",
  {
    # Create test data
    mpf <- make_egor(5, 50)
    mpf$alter <- mpf$alter[1:20,]
    mpf$aatie <- mpf$aatie[4:20,]
    

    expect_error(clustered_graphs(mpf, "country"), NA)
    
  }
)

test_that(
  "Methods work (properly) with NAs in grouping variable.",
  {
    # Create test data
    mpf <- make_egor(5, 50)
    mpf$alter$country[sample(1:20, 3)] <- NA 
    
    expect_error(clustered_graphs(mpf, "country"), NA)

    mpf$ego$.egoID[[1]]
    b <- igraph::V(clustered_graphs(mpf, "country")[[1]])$grp.size
    c <- table(mpf$alter %>% filter(.egoID == 1) %>% pull(country)) %>% as.vector()
    expect_equal(b, c)
    
  }
)

test_that(
  "Methods work (properly) with grouping variable being completly NA.",
  {
    # Create test data
    mpf <- make_egor(5, 50)
    mpf$alter$country <- NA
    
    expect_error(clustered_graphs(object = mpf, clust.groups = "country"), NA)
    
    
    expect_null(igraph::V(clustered_graphs(mpf, "country")[[1]])$grp.size)
  }
)

# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                    labels = T, to.pdf = F)
# 
# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                    labels = F, to.pdf = T)