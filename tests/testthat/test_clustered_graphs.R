context("test_clustered_graphs.R")


test_that(
  "Methods are working.",
  {
  # Create test data
  mpf <- make_egor(5, 50)
  of2 <- as_tibble(mpf)
  
  alters <- tidyr::unnest(dplyr::select(of2, egoID, .alts))
  aaties <- tidyr::unnest(dplyr::select(of2, egoID, .aaties))
  
  alts.list <- mpf$.alts
  aaties.list <- mpf$.aaties
  
  expect_error(clustered_graphs(mpf, "country"), NA)
  expect_error(clustered_graphs(alters, aaties, "country"), NA)
  expect_error(clustered_graphs(alts.list, aaties.list, "country"), NA)
  }
)

test_that(
  "Methods are working with partially missing data.",
  {
    # Create test data
    mpf <- make_egor(5, 50)
    mpf$.alts[[1]] <- mpf$.alts[[1]][0,]
    mpf$.aaties[[1]] <- mpf$.aaties[[1]][0,]
    
    of2 <- as_tibble(mpf)
    
    alters <- tidyr::unnest(dplyr::select(of2, egoID, .alts))
    aaties <- tidyr::unnest(dplyr::select(of2, egoID, .aaties))
    
    alts.list <- mpf$.alts
    aaties.list <- mpf$.aaties 
    
    expect_error(clustered_graphs(mpf, "country"), NA)
    expect_error(clustered_graphs(alters, aaties, "country"), NA)
    expect_error(clustered_graphs(alts.list, aaties.list, "country"), NA)
  }
)

test_that(
  "Methods work (properly) with NAs in grouping variable.",
  {
    # Create test data
    mpf <- make_egor(5, 50)
    a <- mpf$.alts[[1]]$country
    mpf$.alts[[1]]$country[sample(1:length(a), 5)] <- NA 

    of2 <- as_tibble(mpf)
    
    alters <- tidyr::unnest(dplyr::select(of2, egoID, .alts))
    aaties <- tidyr::unnest(dplyr::select(of2, egoID, .aaties))
    
    alts.list <- mpf$.alts
    aaties.list <- mpf$.aaties 
    
    expect_error(clustered_graphs(mpf, "country"), NA)
    expect_error(clustered_graphs(alters, aaties, "country"), NA)
    expect_error(clustered_graphs(alts.list, aaties.list, "country"), NA)
    
    b <- igraph::V(clustered_graphs(mpf, "country")[[1]])$grp.size
    c <- table(mpf$.alts[[1]]$country) %>% as.vector()
    expect_equal(b, c)
    
  }
)

test_that(
  "Methods work (properly) with grouping variable being completly NA.",
  {
    # Create test data
    mpf <- make_egor(5, 50)
    mpf$.alts[[1]]$country <- NA
    
    of2 <- as_tibble(mpf)
    
    alters <- tidyr::unnest(dplyr::select(of2, egoID, .alts))
    aaties <- tidyr::unnest(dplyr::select(of2, egoID, .aaties))
    
    alts.list <- mpf$.alts
    aaties.list <- mpf$.aaties 
    
    expect_error(clustered_graphs(object = mpf, clust.groups = "country"), NA)
    expect_error(clustered_graphs(alters, aaties, "country"), NA)
    expect_error(clustered_graphs(object = alts.list,aaties =  aaties.list, clust.groups = "country"), NA)
    
    expect_null(igraph::V(clustered_graphs(mpf, "country")[[1]])$grp.size)
  }
)

# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                    labels = T, to.pdf = F)
# 
# vis_clustered_graphs(graphs, node.min.size = 45, node.max.size = 200,
#                    labels = F, to.pdf = T)