# Network Visualisation Wizzard

#' Network Visualisation Wizzard (egor)
#'
#' This function uses an 'egor' object (list of data objects created by an 
#' egor import function), extracts the graphs object and uses it to visualise
#' all networks contained in an interactive Browser Application (R-Shiny).
#' @param object An egor object.
#' @param shiny_opts `List` of arguments to be passed to `shinyApp()`'s options argument. 
#' @return Opens an interactive Browser Application.
#' @examples 
#' \dontrun{
#' data("egor32")
#' egor_vis_wizzard(egor32)
#' }
#' @keywords ego-centric network analysis
#' @export
#' @import shiny
#' @importFrom igraph get.vertex.attribute
#' @importFrom igraph get.edge.attribute
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom grDevices blues9 grey grey.colors heat.colors rainbow topo.colors
#' @importFrom graphics legend text
#' @importFrom tibble as_tibble
egor_vis_wizzard <- function(object, shiny_opts = list(launch.browser = TRUE)) {
  

# App Globals -------------------------------------------------------------

  egors <- ls(envir = .GlobalEnv)[sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), function(x) class(x)[1] == "egor")]
  col_pal_names <- c("Heat Colors", "Yellow-Green", "Red-Yellow", "Blue-Red", "Black-White", "Greys", "Rainbow", "Topo Colors")
  

  
  graphs <- as_igraph(object)
  
  object <- as_tibble(object)

    
    for(i in 1:round(length(graphs))) {
      if(!length(V(graphs[[i]])) < 1) {
        #' @importFrom igraph list.vertex.attributes
        try(v.atts <- list.vertex.attributes((graphs[[i]])))
        #' @importFrom igraph list.edge.attributes
        try(e.atts <- list.edge.attributes((graphs[[i]])))
        break()
      }
    }
    
    
    
    v.atts <- make_select_vector(v.atts)
    e.atts <- make_select_vector(e.atts)
  
  
  result_names <- names(object)

  
  shinyApp(
    ui = fluidPage(
      
      # Application title
      titlePanel("Network Visualisation Wizzard"),
      mainPanel(
        plotOutput("Plot", width = "100%", height = "600px")
      ),
      
      # Sidebar
      mainPanel(
        fluidRow(
          # Sidebar bottom left
          sidebarPanel(
            selectInput("egor", "Select egor object", egors),
            
            numericInput("nnumber", "Network No.:", step = 1, min = 1,
                         max = round(length(graphs), digits = 0), value = 1),
            sliderInput("zoom_factor_e",label = "Zoom Edges:", min = 1, max = 10, value = 3, step = .1),
            sliderInput("zoom_factor_v",label = "Zoom Vertices:", min = 1, max = 20, value = 3, step = .1)
            
          ),
          # Sidebar bottom tabs
          mainPanel(
            tabsetPanel(
              tabPanel("Vertices",
                        fluidRow(
                          column(6,
                                 selectInput("v.size", "Vertex Size:", choices = v.atts),
                                 selectInput("v.color", "Vertex Color:", choices = v.atts),
                                 selectInput("v.color_pal", "Color Palette:", choices = col_pal_names)),
                          column(6,
                                 selectInput("v.label", "Vertex Labels:", choices = v.atts),
                                 textInput("l.label", "Legend Label:"))
                        )),
              tabPanel("Edges",
                       column(6,
                              selectInput("e.width", "Edge Width:", choices = e.atts),
                              selectInput("e.color", "Edge Color:", choices = e.atts)),
                       column(6,
                              selectInput("e.color_pal", "Color Palette:", choices = col_pal_names))
              ),
              tabPanel("Results",
                       selectInput("disp.results", "Results 3:", choices = result_names, multiple = TRUE)
                       
              ),tabPanel("Export",
                         downloadButton("save_plot", label = "Save this Plot"),
                         downloadButton("save_all_plots", label = "Save all Plots"),
                         downloadButton("save_egor", label = "Save egor object with igraphs (incl. plotting parameters)")
                         
              ))))
      )
    ),
    server = function(input, output) {
      
      output$Plot <- renderPlot({
        nnumber <- input$nnumber
        plot_graph(nnumber, graphs, input, object)
      })
      
      output$save_all_plots <- downloadHandler(
        filename = function() {
          paste("plots_export", "pdf", sep=".")
        },
        content = function(file){
          pdf(file, width = 9, onefile = TRUE)
          for (i in 1:length(graphs)) {
            plot_graph(i, graphs, input, object)
          }
          dev.off()
        }
      )
      
      output$save_plot <- downloadHandler(
        filename = function() {
          paste(input$nnumber, "pdf", sep=".")
        },
        content = function(file){
          pdf(file, width = 9)
          plot_graph(input$nnumber, graphs, input, object)
          dev.off()
        }
      )
      
      output$save_egor <- downloadHandler(
        filename = function() {
          paste("egor_export", "Rda", sep = ".")
        },
        content = function(file) {
          gg <- map(1:length(graphs), function(x) {
            pp <- collect_plot_params(x, graphs, input)
            V(graphs[[x]])$size <- pp$vertex.size
            V(graphs[[x]])$color <- pp$colors_[pp$vertex.color]
            V(graphs[[x]])$label <- pp$vertex.label
            E(graphs[[x]])$width <- pp$edge.width
            E(graphs[[x]])$color <- pp$e_colors[pp$edge.color]
            graphs[[x]]$layout <- pp$layout_
            graphs[[x]]
          })
          object$graphs <- gg
          class(object) <- c("egor", class(object))
          save(object, file = file)
        }
      )
      
      output$object_to_envir <- downloadHandler(
        filename = function() {
          paste("egor_export", "Rda", sep = ".")
        },
        content = function(file) {
          object$graphs <- graphs
          class(object) <- c("egor", class(object))
          save(object, file = file)
        }
      )
      
    }, options = shiny_opts)
}

make_select_vector <- function(x) {
  c("-Select Entry-",x)
}


# Server Functions --------------------------------------------------------

egor_col_pal <- function(pal_name = "Heat Colors", n) {
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

collect_plot_params <- function(nnumber, graphs, input) {
  # Default Colors
  colors_ <- blues9
  e_colors <- grey(0.6)
  
  # Vertex Size
  if(input$v.size != "-Select Entry-") {
    vertex.size <- as.numeric(as.factor(get.vertex.attribute(graphs[[nnumber]], input$v.size)))
    vertex.size[is.na(vertex.size)] <- 0.1
    vertex.size <- vertex.size * 4 + input$zoom_factor_v/2
  } else {
    vertex.size <- rep(5, length(V(graphs[[nnumber]]))) * 4 + input$zoom_factor_v/2
  }
  
  # Vertex Color
  if(input$v.color != "-Select Entry-") {
    vertex.color <- get.vertex.attribute(graphs[[nnumber]], input$v.color)
    vertex.color[is.na(vertex.color)] <- 0
    vertex.color <- factor(vertex.color)
    colors_ <- egor_col_pal(input$v.color_pal, 
                            length(levels(factor(get.vertex.attribute(graphs[[nnumber]], 
                                                                      input$v.color)))))
  } else {
    vertex.color <- 1
  }
  
  # Edge Width
  if(input$e.width != "-Select Entry-") {
    edge.width <- get.edge.attribute(graphs[[nnumber]], input$e.width) * input$zoom_factor_e
    edge.width[is.na(edge.width)] <- 0
  } else {
    edge.width <- rep(1, length(E(graphs[[nnumber]]))) * input$zoom_factor_e
  }
  
  # Edge Color
  if(input$e.color != "-Select Entry-") {
    edge.color <- get.edge.attribute(graphs[[nnumber]], input$e.color)
    edge.color[is.na(edge.color)] <- 0
    edge.color <- as.numeric(factor(edge.color))
    e_colors <- egor_col_pal(input$e.color_pal, 
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
  
  
  #' @importFrom igraph layout.fruchterman.reingold
  layout_ <- layout.fruchterman.reingold(graphs[[nnumber]], weights = edge.width)
  
  # Collect and return all params
  ls_ <- ls()
  ls_ <- ls_[!ls_ %in% c("input", "graphs", "nnumber")]
  res <- mget(ls_)
  print(1)
  names(res) <- ls_
  print(res)
  res
}

plot_graph <- function(nnumber, graphs, input, object) {
  if (!sum(V(graphs[[nnumber]])) > 0) {
    # Plot Error message.
    plot(NA, xlim = c(1,10), ylim = c(0.75,10),  type = "n",  yaxt="n", xaxt="n", ylab="", xlab="", bty="L")
    text(5,1,'No network data available for this entry.')
    return()
  }
  print(3)
  list2env(collect_plot_params(nnumber, graphs, input), environment())
  print(4)
  #' @importFrom igraph plot.igraph
  plot.igraph(
    graphs[[nnumber]],
    vertex.size = vertex.size,
    vertex.color = colors_[vertex.color],
    edge.width = edge.width,
    vertex.label = vertex.label,
    edge.color = e_colors[edge.color],
    layout = layout_
  )
  print(5)
  # Sanitize Variable Names
  sane_disp_results <- gsub("\\.",  " ", names(object))
  sane_disp_results <- gsub("  ",  " ",sane_disp_results)
  sane_disp_results <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", sane_disp_results, perl=TRUE)
  
  # Print results on plot canvas
  y_pos_res = 1.2
  for (result_name in input$disp.results) {
    text(-2 , y_pos_res, 
         paste(sane_disp_results[which(colnames(object) == result_name)],
               ": ", object[nnumber, result_name][[1]], sep = ""), adj = c(0,0))
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
