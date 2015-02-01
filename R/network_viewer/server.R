library(shiny)

# Define server logic required to draw a histogram
shiny::shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  output$distPlot <- shiny::renderPlot({
    x    <- graphs  # Old Faithful Geyser data
    nnumber <- input$bins

    # draw the histogram with the specified number of bins
    as <- sort(unique(V(graphs[[3]])$V165))
    
    
    plot.igraph(graphs[[nnumber]], vertex.size = round(as.numeric(factor(V(graphs[[nnumber]])$V165), ordered = TRUE, levels = as)*10, digits = 0) )
    
  })
})
