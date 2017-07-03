library(shiny)
library(igraph)
library(tibble)

graphs <- egor::to.network(eigor$.aaties, eigor$.alts)
results <- as_tibble(eigor)
colors_ <- blues9
e_colors <- grey(0.6)

# Color palettes




egors <- ls(envir = .GlobalEnv)[sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), function(x) class(x)[1] == "egor")]

shiny::shinyServer(function(input, output) {
  
  egoR_col_pal <- function(pal_name = "Heat Colors", n) {
    if(pal_name == "Heat Colors")  # Heat Colors
      pal <- heat.colors(n)
    if(pal_name == "Yellow-Green") # Yellow-Green
      pal <- rainbow(n, start = 0.2, end = 0.4)
    if(pal_name == "Red-Yellow")  # Red-Yellow
      pal <- rainbow(n, start = 0.0, end = 0.2)
    if(pal_name == "Blue-Red")  # Blue-Red
      pal <- rainbow(n, start = 0.6, end = 1)
    if(pal_name == "Black-White")  # Black-White
      pal <- grey.colors(n, start = 0, end = 1)
    if(pal_name == "Greys")  # Greys
      pal <- grey.colors(n)
    if(pal_name == "Rainbow")  # Rainbow
      pal <- rainbow(n)
    if(pal_name == "Topo Colors")  # Topo Colors
      pal <- topo.colors(n)
    rev(pal)
  }
  
  plot_graph <- function(nnumber, graphs, input, results) {
    if (sum(V(graphs[[nnumber]])) > 0) {
      # Vertex Size
      if(input$v.size != "-Select Entry-") {
        vertex.size <- as.numeric(as.factor(get.vertex.attribute(graphs[[nnumber]], input$v.size)))
        vertex.size[is.na(vertex.size)] <- 0.1
        vertex.size <- vertex.size * input$zoom_factor
      } else {
        vertex.size <- NULL
      }
      
      # Vertex Color
      if(input$v.color != "-Select Entry-") {
        vertex.color <- get.vertex.attribute(graphs[[nnumber]], input$v.color)
        vertex.color[is.na(vertex.color)] <- 0
        vertex.color <- factor(vertex.color)
        colors_ <- egoR_col_pal(input$v.color_pal, 
                                length(levels(factor(get.vertex.attribute(graphs[[nnumber]], 
                                                                          input$v.color)))))
        
      } else {
        vertex.color <- 1
      }
      
      # Edge Width
      if(input$e.width != "-Select Entry-") {
        edge.width <- get.edge.attribute(graphs[[nnumber]], input$e.width) * input$zoom_factor
        edge.width[is.na(edge.width)] <- 0
      } else {
        edge.width <- NULL
      }
      
      # Edge Color
      if(input$e.color != "-Select Entry-") {
        edge.color <- get.edge.attribute(graphs[[nnumber]], input$e.color)
        edge.color[is.na(edge.width)] <- 0
        e_colors <- egoR_col_pal(input$e.color_pal, 
                                length(levels(factor(get.edge.attribute(graphs[[nnumber]], 
                                                                          input$e.color)))))
      } else {
        edge.color <- 1
      }
      
      # Label
      if(input$v.label != "-Select Entry-") {
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
    lll <- igraph::layout.fruchterman.reingold(graphs[[nnumber]], weights = edge.width)
    
    igraph::plot.igraph(graphs[[nnumber]], vertex.size = vertex.size, 
                        vertex.color = colors_[vertex.color], edge.width = edge.width,
                        vertex.label = vertex.label, edge.color=e_colors[edge.color],
                        layout = lll)
    
    # Sanitize Variable Names
    sane_disp_results <- gsub("\\.",  " ", names(results))
    sane_disp_results <- gsub("  ",  " ",sane_disp_results)
    sane_disp_results <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", sane_disp_results, perl=TRUE)
    
    # Print results on plot canvas
    y_pos_res = 1.2
    for (result_name in input$disp.results) {
      text(-2 , y_pos_res, 
           paste(sane_disp_results[which(colnames(results) == result_name)],
          ": ", results[nnumber, result_name], sep = ""), adj = c(0,0))
      y_pos_res = y_pos_res - 0.2
    }
    
    # Legend
    if(input$v.color != "-Select Entry-") {
      color_var <- get.vertex.attribute(graphs[[nnumber]], input$v.color)
      color_var[is.na(color_var)] <- 0
      title_ <- ifelse(input$l.label == "", input$v.color, input$l.label)
      legend(x =-2, y = -0.8, legend= levels(factor(color_var)), pt.bg = colors_, pt.cex = 1.5,  pch = 22, bty ="n", y.intersp = 1, title = title_)
    }
  
  }

  output$Plot <- shiny::renderPlot({
    #x    <- graphs  
    nnumber <- input$nnumber
    plot_graph(nnumber, graphs, input, results)

    output$save_all_plots <- downloadHandler(
      filename = function() {
        paste(nnumber, "pdf", sep=".")
      },
      content = function(file){
        pdf(file, width = 9, onefile = T)
        for (i in 1:length(graphs)) {
          plot_graph(i, graphs, input, results)
        }
        dev.off()
      }
      
    )
    
    output$save_plot <- downloadHandler(
      filename = function() {
        paste(nnumber, "pdf", sep=".")
      },
      content = function(file){
        pdf(file, width = 9)
          plot_graph(nnumber, graphs, input, results)
        dev.off()
      }
      
    )
    
  }, width = 900)
})

