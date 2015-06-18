library(shiny)
library(igraph)

shiny::shinyServer(function(input, output) {


  plot_graph <- function(nnumber, graphs, input) {
    if (sum(V(graphs[[nnumber]])) > 0) {
      # Vertex Size
      if(input$v.size != "name") {
        vertex.size <- get.vertex.attribute(graphs[[nnumber]], input$v.size)
        vertex.size[is.na(vertex.size)] <- 0.1
        vertex.size <- vertex.size * input$zoom_factor
      } else {
        vertex.size <- NULL
      }
      
      # Vertex Color
      if(input$v.color != "name") {
        vertex.color <- get.vertex.attribute(graphs[[nnumber]], input$v.color)
        vertex.color[is.na(vertex.color)] <- 0
      } else {
        vertex.color <- NULL
      }
      
      # Edge Width
      if(input$e.width != "name") {
        edge.width <- get.edge.attribute(graphs[[nnumber]], input$e.width) * 5
        edge.width[is.na(edge.width)] <- 0
      } else {
        edge.width <- NULL
      }     
    } else {
        # Plot Error message.
        plot(NA, xlim = c(1,10), ylim = c(0.75,10),  type = "n",  yaxt="n", xaxt="n", ylab="", xlab="", bty="L")
        text(5,1,'No network data available for this entry.')
    }
      igraph::plot.igraph(graphs[[nnumber]], vertex.size = vertex.size, vertex.color = vertex.color, edge.width = edge.width)
  }
  
  output$Plot <- shiny::renderPlot({
    #x    <- graphs  
    nnumber <- input$bins
    plot_graph(nnumber, graphs, input)
      


    
    output$save_all_plots <- downloadHandler(
      filename = function() {
        paste(nnumber, "pdf", sep=".")
      },
      content = function(file){
        #if(input$png_pdf == "png")
        #  png(file)
        #else
        pdf(file)
        #layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
        for (i in 1:length(graphs)) {
          plot_graph(i, graphs, input)
        }
        dev.off()
      }
      
    )
    
    output$save_plot <- downloadHandler(
      filename = function() {
        paste(nnumber, "pdf", sep=".")
      },
      content = function(file){
        pdf(file)
          plot_graph(nnumber, graphs, input)
        dev.off()
      }
      
    )
    
  })
})

