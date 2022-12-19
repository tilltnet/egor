context("test_clustered_graphs.R")

test_that(
  "Methods are working.",
  {
  # Create test data
  mpf <- make_egor(5, 50)

  expect_error(egor:::clustered_graphs.egor(object = mpf,
                                clust.groups = "country"), NA)
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

if (FALSE) {
  mpf <- make_egor(5, 50)
  mpf <- 
    mpf |> 
    activate(alter) |> 
    filter(.egoID == 2 & country != "Australia" | .egoID != 2)
  graphs <- clustered_graphs(object = mpf, clust.groups = "country")
  
  # remove one node to test if it influences remaining node positions; it does!
  graphs[[1]] <- graphs[[1]] |> tidygraph::as_tbl_graph() |> filter(name != "Australia")
  graphs[[2]] <- graphs[[2]] |> tidygraph::as_tbl_graph() |> filter(name != "USA")
  graphs[[4]] <- graphs[[4]] |> tidygraph::as_tbl_graph() |> filter(name != "Poland")
  vis_clustered_graphs(
    graphs,
    node.size.multiplier = 3,
    edge.width.multiplier = 3,
    labels = T,
    to.pdf = F
  )

  vis_clustered_graphs(
    graphs,
    node.size.multiplier = 2,
    edge.width.multiplier = 1,
    labels = F,
    to.pdf = F
  )
  
  par(mfrow = c(2,3))
  vis_clustered_graphs(
    graphs,
    node.size.multiplier = 10,
    normalise.node.sizes = FALSE,
    edge.width.multiplier = 1,
    labels = F,
    to.pdf = F
  )


test_that("vis_clustered_graphs() works when all btw. group density is 0", {
  mpf <- make_egor(5, 50)
  
  graphs <- clustered_graphs(object = mpf, clust.groups = "country")
  
  graphs[[1]] %>% 
    tidygraph::as_tbl_graph() %>% 
    activate(edges) %>% 
    mutate(grp.density = c(0, 0, 0)) %>% 
    tidygraph::as.igraph() %>% 
    list() %>% 
    vis_clustered_graphs(edge.width.multiplier = 5,
           normalise.node.sizes = TRUE)
})


test_that("vis_clustered_graphs() works when ???", {
  mpf <- make_egor(5, 50)
  
  graphs <- clustered_graphs(object = mpf, clust.groups = "country")
  graphs <- 
    graphs[[1]] %>% 
    tidygraph::as_tbl_graph() %>% 
    activate(edges) %>% 
    filter(FALSE) %>% 
    tidygraph::as.igraph() %>% 
    list() 
    vis_clustered_graphs(graphs,
                         edge.width.multiplier = 5,
                         normalise.node.sizes = TRUE)
    
    
    node.size.multiplier = 1
    node.min.size = 0
    node.max.size = 200
    normalise.node.sizes = TRUE
    edge.width.multiplier = 1
    center = 1
    label.size = 0.8
    labels = FALSE
    legend.node.size = 45
    pdf.name = NULL
    graph <- graphs[[1]]
    betw_grp_dens <- igraph::E(graphs[[2]])$grp.density
    
    
    gray(1 - ifelse(
      betw_grp_dens  == 0 | length(betw_grp_dens) == 0, 0, betw_grp_dens / max(betw_grp_dens)
    ), alpha = 0.7)
})

}

