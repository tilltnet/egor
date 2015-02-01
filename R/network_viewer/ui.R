library(shiny)
library(igraph)

# Define UI for application that draws a histogram
shiny::shinyUI(fluidPage(

  # Application title
  shiny::titlePanel("Network Viewer"),

  # Sidebar with a slider input for the number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::numericInput("bins",
                  "Network No.:",
                  step = 1,
                  min = 1,
                  max = round(length(graphs), digits = 0),
                  value = 30)
    ),

    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::plotOutput("distPlot")
    )
  )
))



E(graphs[[3]])$weight

dyads.l[[3]]
