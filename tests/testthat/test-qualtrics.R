test_that("import code in qualtrics vignette works", {
  
  
  
  qu_data <- read.csv(system.file("qualtrics", "qualtrics_responses.csv", package = "egor")) 
  
  qu_data <- qu_data[3:nrow(qu_data),]

  # Create egoID
  qu_data$egoID <- 1:nrow(qu_data)
  
  #library(egor)
  
  expect_warning({
    e1 <- onefile_to_egor(egos = qu_data,
                          ID.vars = list(ego = "egoID"),
                          attr.start.col = "A1Gender", # Name of Variable with the first alter attribute
                          attr.end.col = "X5.5", # Name of variable with last alter attribute
                          max.alters = 5, # Number of maximum alters that were named by participants
                          aa.first.var = "AP1") # Name of first alter-alter variable
  })
    
  alter_filter <- 
    e1 %>% 
    as_tibble() %>% 
    arrange(.egoID) %>% 
    select(AlterList_1:AlterList_5) %>% 
    mutate(across(.fns = ~. != "")) %>% 
    as.data.frame() %>% 
    tidyr::pivot_longer(cols = everything()) %>% 
    pull(value)
  
  e1 <- 
    e1 %>% 
    activate(alter) %>% 
    filter(alter_filter)
  
  e1 <- 
    e1 %>% 
    activate(aatie) %>% 
    filter(weight != "")
})
