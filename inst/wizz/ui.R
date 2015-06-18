library(shiny)
library(igraph)
#library(RColorBrewer)
#library(colorspace)

#sequential_hcl(n = 25, h=1)
#choose_palette()
#display.brewer.all()
#brewer.pal(25, name = "YlOrRd")
#v.atts <- names(kaboom$long.df)[2:length(names(kaboom$long.df))]
#names(v.atts) <- names(kaboom$long.df)[2:length(names(kaboom$long.df))]

for(i in 1:round(length(graphs))) {
  if(!length(V(graphs[[i]])) < 1) {
    e.atts <- list.edge.attributes((graphs[[i]]))
    v.atts <- list.vertex.attributes((graphs[[i]]))
    break()
  }
}

# Define UI for application that draws a histogram
shiny::shinyUI(fluidPage(

  # Application title
  shiny::titlePanel("Network Visualisation Wizzard"),

  # Sidebar with a slider input for the number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::numericInput("bins",
                  "Network No.:",
                  step = 1,
                  min = 1,
                  max = round(length(graphs), digits = 0),
                  value = 1),
      shiny::sliderInput("zoom_factor",label = "Zoom:", min = 1, max = 10, value = 3, step = .1),
      shiny::selectInput("v.size", "Vertex Size:", choices = v.atts),
      shiny::selectInput("v.color", "Vertex Color:", choices = v.atts),
      shiny::selectInput("e.width", "Edge Width:", choices = e.atts)
    ),

    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::plotOutput("Plot"),
      #shiny::radioButtons("png_pdf", "Format:", choices = c("pdf", "png")),
      shiny::downloadButton("save_plot", label = "Save this Plots"),
      shiny::downloadButton("save_all_plots", label = "Save all Plots")
    )
  )
))

