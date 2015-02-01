library(shiny)
library(igraph)

shiny::shinyServer(function(input, output) {


  output$distPlot <- shiny::renderPlot({
    x    <- graphs  
    nnumber <- input$bins

    as <- sort(unique(V(graphs[[3]])$V165))
    
    
    plot.igraph(graphs[[nnumber]], vertex.size = round(as.numeric(factor(V(graphs[[nnumber]])$V165), ordered = TRUE, levels = as)*10, digits = 0) )
    
  })
})
