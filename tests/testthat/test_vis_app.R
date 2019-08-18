context("test_vis_app.R")
library(shiny)

if (FALSE) {
  e <- make_egor(5,5)
  f <- make_egor(50,50)
  egor_vis_app(e)
}

as_nested_egor(e) %>% as_igraph()

as_nested_egor(e) %>%
as_igraph()
x <- as_nested_egor(e)