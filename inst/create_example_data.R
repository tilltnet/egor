library(egor)
library(tidyverse)

# edges_32, alters_32, egos_32

e32 <- make_egor(32, 12, netsize_fixed = TRUE)

e32 %>% 
  activate(aatie) %>% 
  as_tibble() %>% 
  count(.egoID) %>% 
  nrow() # If this is not 32 re run make_egor

e32 %>% 
  activate(alter) %>% 
  as_tibble() %>% 
  count(.egoID) %>% 
  nrow() # If this is not 32 re run make_egor

as_tibble(e32) %>% 
  rename(egoID = .egoID) %>% 
  write.csv2(file = "inst/extdata/egos_32.csv", row.names = FALSE)

e32 %>% 
  activate(alter) %>% 
  as_tibble() %>% 
  rename(egoID = .egoID, alterID = .altID) %>% 
  split(.$egoID) %>% 
  map2(1:length(.),
       ~{
         select(.x, -egoID) %>% 
           write.csv2(paste0("inst/extdata/alters_32/", .y, ".csv"), row.names = FALSE)
       })


e32 %>% 
  activate(aatie) %>% 
  as_tibble() %>% 
  rename(egoID = .egoID, Source = .srcID, Target = .tgtID) %>% 
  split(.$egoID) %>% 
  map2(1:length(.),
       ~{
         select(.x, -egoID) %>% 
           write.csv2(paste0("inst/extdata/edges_32/", .y, ".csv"), row.names = FALSE)
       })
  

