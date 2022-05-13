library(egor)
library(tidyverse)

# Extdata ----

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
  

# Data ----

## egor32

egor32 <- e32
ego_design(egor32) <- 1
egos32 <- egor32$ego$variables
alters32 <- egor32$alter
aaties32 <- egor32$aatie
egor32 <- egor32 |> mutate(sex = factor(sex))
egor32 <- egor32 |> activate(alter) |>  mutate(sex = factor(sex)) |> activate(ego)

egos32 <- egos32 |> rename_with(toupper, .cols = starts_with("."))
alters32 <- alters32 |> rename_with(toupper, .cols = starts_with("."))
aaties32 <- aaties32 |> rename_with(toupper, .cols = starts_with("."))

usethis::use_data(egor32, egos32, alters32, aaties32, overwrite = TRUE)


# reduce egos_8, alters_8 and alters_8_wide to 16 egos

alters_8_wide |> 
  select(-...1) |> 
  slice(1:16) |> 
  write_csv2("inst/extdata/alters_8_wide.csv")

alters_8 |> 
  filter(egoID %in% 1:16) |> 
  write_csv2("inst/extdata/alters_8.csv")

egos_8 |> 
  slice(1:16) |> 
  write_csv2("inst/extdata/egos_8.csv")

egos_8 |> 
  slice(1:16) |> 
  write_csv2("inst/extdata/one_file_8.csv")
