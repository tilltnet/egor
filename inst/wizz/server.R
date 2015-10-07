library(shiny)
library(igraph)

shiny::shinyServer(function(input, output) {
  
  plot_graph <- function(nnumber, graphs, input) {
    if (sum(V(graphs[[nnumber]])) > 0) {
      # Vertex Size
      if(input$v.size != "name") {
        vertex.size <- as.numeric(get.vertex.attribute(graphs[[nnumber]], input$v.size))
        vertex.size[is.na(vertex.size)] <- 0.1
        vertex.size <- vertex.size * input$zoom_factor
      } else {
        vertex.size <- NULL
      }
      
      # Vertex Color
      if(input$v.color != "name") {
        vertex.color <- get.vertex.attribute(graphs[[nnumber]], input$v.color)
        vertex.color[is.na(vertex.color)] <- 0
        vertex.color <- factor(vertex.color)
      } else {
        vertex.color <- NULL
      }
      
      # Edge Width
      if(input$e.width != "name") {
        edge.width <- get.edge.attribute(graphs[[nnumber]], input$e.width) * input$zoom_factor
        edge.width[is.na(edge.width)] <- 0
      } else {
        edge.width <- NULL
      }
      # Label
      if(input$v.label != "name") {
        vertex.label <- get.vertex.attribute(graphs[[nnumber]], input$v.label)
        vertex.label[is.na(vertex.label)] <- 0
      } else {
        vertex.label <- V(graphs[[nnumber]])
      }
    } else {
        # Plot Error message.
        plot(NA, xlim = c(1,10), ylim = c(0.75,10),  type = "n",  yaxt="n", xaxt="n", ylab="", xlab="", bty="L")
        text(5,1,'No network data available for this entry.')
    }
      colors_ <- blues9
      igraph::plot.igraph(graphs[[nnumber]], vertex.size = vertex.size, 
                          vertex.color = colors_[vertex.color], edge.width = edge.width,
                          vertex.label = vertex.label)
  }
  
  output$Legend <- shiny::renderPlot({
    if(input$v.color != "name") {
      nnumber <- input$bins
      color_var <- get.vertex.attribute(graphs[[nnumber]], input$v.color)
      color_var[is.na(color_var)] <- 0
      layout(rbind(1,1), heights=c(1,4))
      par(mar=c(0, 0, 0, 0))
      plot.new()
      colors_ <- blues9
      legend(x =0.1, y = 1, legend= levels(factor(color_var)), pt.bg = colors_, pt.cex = 1.5,  pch = 22, bty ="n")
    }

    
    
  })
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

