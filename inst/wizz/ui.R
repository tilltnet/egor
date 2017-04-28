library(shiny)
library(igraph)

graphs <- graphs_
results <- results_

for(i in 1:round(length(graphs))) {
  if(!length(V(graphs[[i]])) < 1) {
    e.atts <- list.edge.attributes((graphs[[i]]))
    v.atts <- list.vertex.attributes((graphs[[i]]))
    break()
  }
}

make_select_vector <- function(x) {
  c("-Select Entry-",x)
}

v.atts <- make_select_vector(v.atts)
e.atts <- make_select_vector(e.atts)


col_pal_names <- c("Heat Colors", "Yellow-Green", "Red-Yellow", "Blue-Red", "Black-White", "Greys", "Rainbow", "Topo Colors")

result_names <- names(results)
shiny::shinyUI(fluidPage(

  # Application title
  shiny::titlePanel("Network Visualisation Wizzard"),
  shiny::mainPanel(
    shiny::plotOutput("Plot", width = "100%", height = "600px")
  ),

  # Sidebar
  shiny::mainPanel(
    
    shiny::fluidRow(
    shiny::sidebarPanel(
      
        shiny::numericInput("nnumber",
                            "Network No.:",
                            step = 1,
                            min = 1,
                            max = round(length(graphs), digits = 0),
                            value = 1),
        shiny::sliderInput("zoom_factor",label = "Zoom:", min = 1, max = 10, value = 3, step = .1),
        shiny::downloadButton("save_plot", label = "Save this Plot"),
        shiny::downloadButton("save_all_plots", label = "Save all Plots")
      
    ),
  shiny::mainPanel(
    shiny::tabsetPanel(
      
      shiny::tabPanel("Vertices",
      shiny::fluidRow(
        column(6,
          shiny::selectInput("v.size", "Vertex Size:", choices = v.atts),
          shiny::selectInput("v.color", "Vertex Color:", choices = v.atts),
          shiny::selectInput("v.color_pal", "Color Palette:", choices = col_pal_names)),
        column(6,
          shiny::selectInput("v.label", "Vertex Labels:", choices = v.atts),
          shiny::textInput("l.label", "Legend Label:"))
      )),
    shiny::tabPanel("Edges",
      shiny::selectInput("e.width", "Edge Width:", choices = e.atts),
      shiny::selectInput("e.color", "Edge Color:", choices = e.atts),
      shiny::selectInput("e.color_pal", "Color Palette:", choices = col_pal_names)
    ),
    shiny::tabPanel("Results",
      shiny::selectInput("disp.results", "Results 3:", choices = result_names, multiple = T)
      
    ))))

    # Network Plot

  )
))

