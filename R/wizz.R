if(getRversion() >= "2.15.1") utils::globalVariables(c("clrs", "colors_", "e_colors", "edge.color", "edge.width", "graphs", "layout_", "vertex.label", "vertex.size"))

#' `egor` Network Visualization App
#'
#' Launches an interactive Shiny Web App, that creates a list of
#' `igraph` objects from an 'egor' object and offers the user several graphical
#' means of interacting with the visualization parameters for all networks in 
#' the `egor` object.
#' @param object An `egor` object.
#' @param shiny_opts `List` of arguments to be passed to `shinyApp()`'s options argument.
#' @examples 
#' if(interactive()){
#'   data("egor32")
#'   egor_vis_app(egor32)
#' }
#' @keywords ego-centered network analysis
#' @export
#' @import shiny
#' @importFrom igraph get.vertex.attribute
#' @importFrom igraph get.edge.attribute
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom grDevices blues9 grey grey.colors heat.colors rainbow topo.colors
#' @importFrom graphics legend text
#' @importFrom tibble as_tibble
#' @importFrom igraph list.edge.attributes
#' @importFrom igraph list.vertex.attributes
#' @importFrom igraph V<-
#' @importFrom igraph E<-
egor_vis_app <- function(object, shiny_opts = list(launch.browser = TRUE)) {
  
# App Globals -------------------------------------------------------------
  egors <- ls(envir = .GlobalEnv)[sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), function(x) class(x)[1] == "egor")]
  col_pal_names <- c("Heat Colors", "Yellow-Green", "Red-Yellow", "Blue-Red", "Black-White", "Greys", "Rainbow", "Topo Colors")
  shiny_opts <- c(shiny_opts, width="20")
  object_enex <- as.character(enexpr(object))
  
  shinyApp(
    
# UI ----------------------------------------------------------------------

ui = fluidPage(
  title  = "egor's Network Visualization App",
  
  
  fluidRow(width = 12,
           plotOutput(
             "Plot", width = "100%", height = "600px"
           )),
  
  # Sidebar
  fluidRow(
    width = 12,
    column(
      3,
      selectInput("egor",
                  "Select `egor` object",
                  egors,
                  selected = object_enex),
      uiOutput("choose_nnumber"),
      tags$div(
        title = "When including ego make sure that corresponding ego and alter variables have equal names in the `egor` object.",
          tags$div(checkboxInput("include.ego",
                                 "Include Ego",
                                 FALSE),
                   style = "float: left;"),
          tags$div(icon("question-circle"),
                   style = "float: left; margin-top: 12px; margin-left: -200px;")
        )
      
    ),
    column(
      3,
      
      sliderInput(
        "zoom_factor_e",
        label = "Edge Width:",
        min = 1,
        max = 10,
        value = 3,
        step = .1
      ),
      sliderInput(
        "zoom_factor_v",
        label = "Vertex Size:",
        min = 0,
        max = 75,
        value = 25,
        step = .1
      )
    ),
    column(6,
           tabsetPanel(
             tabPanel("Vertices",
                      fluidRow(
                        column(
                          6,
                          uiOutput("choose_v.size"),
                          uiOutput("choose_v.color"),
                          selectInput("v.color_pal",
                                      "Color Palette:",
                                      choices = col_pal_names)
                        ),
                        column(
                          6,
                          uiOutput("choose_v.label"),
                          textInput("l.label", "Legend Label:")
                        )
                      )),
             tabPanel(
               "Edges",
               column(6,
                      uiOutput("choose_e.width"),
                      uiOutput("choose_e.color")),
               column(
                 6,
                 selectInput("e.color_pal", "Color Palette:", choices = col_pal_names)
               )
             ),
             tabPanel("Results",
                      uiOutput("choose_disp.results")),
             tabPanel(
               "Export",
               downloadButton("save_plot", label = "Save this Plot"),
               downloadButton("save_all_plots", label = "Save all Plots"),
               downloadButton("save_egor", label = "Save `egor` object with igraphs (incl. plotting parameters)")
               
             )
           ))
    
  ),
  fluidRow(
    column(12, inputPanel(tags$div(style = "height: 250px; width: 100%;",
                                   tags$p("egor's Network Visualization App"))))
  )
), 

# SERVER ------------------------------------------------------------------

    server = function(input, output) {
      obj <- reactive({
        get(input$egor, envir = .GlobalEnv) %>% 
          new_egor_to_old()
      })

      result_names <- reactive({
        rn <- names(obj())
        rn[!rn %in% c(".alts", ".aaties")]
      })
      
      graphs <- reactive({
        shared_names <- result_names()[result_names() %in% names(obj()$.alts[[1]])]
        as_igraph(obj(), 
                  include.ego = input$include.ego,
                  ego.attrs = shared_names)
        })
      
      #object <- as_tibble(object)

      v.atts <- reactive(make_select_vector(
        list.vertex.attributes((graphs()[[1]]))))
      e.atts <- reactive(make_select_vector(
        list.edge.attributes((graphs()[[1]]))))
         
      output$choose_nnumber <- renderUI({
      numericInput("nnumber","Network No.:",step = 1,min = 1, 
                   max = length(graphs()), value = 1)
      })
      
      output$choose_v.size <- renderUI({
        selectInput("v.size", "Vertex Size:", choices = v.atts())
      })
      
      output$choose_v.color <- renderUI({
        selectInput("v.color", "Vertex Color:", choices = v.atts())
      })

      output$choose_v.label <- renderUI({
        selectInput("v.label", "Vertex Labels:", choices = v.atts())
      })
      
      output$choose_e.width <- renderUI({
        selectInput("e.width", "Edge Width:", choices = e.atts())
      })
      
      output$choose_e.color <- renderUI({
        selectInput("e.color", "Edge Color:", choices = e.atts())
      })
      
      output$choose_disp.results <- renderUI({
        selectInput("disp.results", "Results 3:", choices = result_names(), multiple = TRUE)
      })
      
      values <- reactiveValues()
      values$v.size <- "-Select Entry-"
      values$v.color <- "-Select Entry-"
      values$v.label <- "-Select Entry-"
      values$e.width <- "-Select Entry-"
      values$e.color <- "-Select Entry-"
      values$nnumber <- 1
      values$disp.results <- c()
      
      observeEvent(input$v.size, {
        values$v.size <- input$v.size
      })
      observeEvent(input$v.color, {
        values$v.color <- input$v.color
      })
      observeEvent(input$v.label, {
        values$v.label <- input$v.label
      })
      observeEvent(input$e.width, {
        values$e.width <- input$e.width
      })
      observeEvent(input$e.color, {
        values$e.color <- input$e.color
      })
      observeEvent(input$nnumber, {
        values$nnumber <- input$nnumber
      })
      observeEvent(input$disp.results, {
        values$disp.results <- input$disp.results
      })
      
      output$Plot <- renderPlot({
        nnumber <- values$nnumber
        plot_graph(nnumber, graphs(), input, obj(), values)
      })
      
      output$save_all_plots <- downloadHandler(
        filename = function() {
          paste("plots_export", "pdf", sep=".")
        },
        content = function(file){
          pdf(file, width = 9, onefile = TRUE)
          for (i in 1:length(graphs())) {
            plot_graph(i, graphs(), input, obj(), values)
          }
          dev.off()
        }
      )
      
      output$save_plot <- downloadHandler(
        filename = function() {
          paste(values$nnumber, "pdf", sep=".")
        },
        content = function(file){
          pdf(file, width = 9)
          plot_graph(values$nnumber, graphs(), input, obj(), values)
          dev.off()
        }
      )
      
      output$save_egor <- downloadHandler(
        filename = function() {
          paste("egor_export", "Rda", sep = ".")
        },
        content = function(file) {
          gg <- purrr::map(1:length(graphs()), function(x) {
            pp <- collect_plot_params(x, graphs(), input, values)
            grph <- graphs()[[x]]
            V(grph)$size <- pp$vertex.size
            V(grph)$color <- pp$clrs
            V(grph)$label <- pp$vertex.label
            E(grph)$width <- pp$edge.width
            E(grph)$color <- pp$e_colors[pp$edge.color]
            grph$layout <- pp$layout_
            grph
          })
          egor_obj <- obj()
          egor_obj$graphs <- gg
          save(egor_obj, file = file)
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

collect_plot_params <- function(nnumber, graphs, input, values) {
  # Default Colors
  colors_ <- blues9
  e_colors <- grey(0.6)
  
  # Vertex Size
  if(values$v.size != "-Select Entry-") {
    vertex.size <- as.numeric(as.factor(get.vertex.attribute(graphs[[nnumber]], values$v.size)))
    vertex.size[is.na(vertex.size)] <- 0.1
    vertex.size <- vertex.size * 4 + input$zoom_factor_v/2
  } else {
    vertex.size <- rep(5, length(V(graphs[[nnumber]]))) * 4 + input$zoom_factor_v
  }

  # Vertex Color
  if(values$v.color != "-Select Entry-") {
    vertex.color <- get.vertex.attribute(graphs[[nnumber]], values$v.color)
    #vertex.color[is.na(vertex.color)] <- 0
    vertex.color <- factor(vertex.color)
    colors_ <- egor_col_pal(input$v.color_pal, 
                            length(levels(factor(get.vertex.attribute(graphs[[nnumber]], 
                                                                      values$v.color)))))
    clrs <- colors_[vertex.color]
    clrs[is.na(clrs)] <- "#ffffff"
  } else {
    vertex.color <- 1
    clrs <- "#eeeeff"
  }
  
  # Edge Width
  if(values$e.width != "-Select Entry-") {
    edge.width <- get.edge.attribute(graphs[[nnumber]], values$e.width) * input$zoom_factor_e
    #edge.width[is.na(edge.width)] <- 0
  } else {
    edge.width <- rep(1, length(E(graphs[[nnumber]]))) * input$zoom_factor_e
  }
  
  # Edge Color
  if(values$e.color != "-Select Entry-") {
    edge.color <- get.edge.attribute(graphs[[nnumber]], values$e.color)
    #edge.color[is.na(edge.color)] <- 0
    edge.color <- as.numeric(factor(edge.color))
    e_colors <- egor_col_pal(input$e.color_pal, 
                             length(levels(factor(get.edge.attribute(graphs[[nnumber]], 
                                                                     values$e.color)))))
  } else {
    edge.color <- 1
  }
  
  # Label
  if(values$v.label != "-Select Entry-") {
    vertex.label <- get.vertex.attribute(graphs[[nnumber]], values$v.label)
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
  names(res) <- ls_
  res
}

plot_graph <- function(nnumber, graphs, input, object, values) {
  if (!sum(V(graphs[[nnumber]])) > 0) {
    # Plot Error message.
    plot(NA, xlim = c(1,10), ylim = c(0.75,10),  type = "n",  yaxt="n", xaxt="n", ylab="", xlab="", bty="L")
    text(5,1,'No network data available for this entry.')
    return()
  }
  list2env(collect_plot_params(nnumber, graphs, input, values), environment())
  #' @importFrom igraph plot.igraph
  set.seed(1)
  plot.igraph(
    graphs[[nnumber]],
    vertex.size = vertex.size,
    vertex.color = clrs,
    edge.width = edge.width,
    vertex.label = vertex.label,
    edge.color = e_colors[edge.color],
    layout = layout_
  )
  # Sanitize Variable Names
  sane_disp_results <- gsub("\\.",  " ", names(object))
  sane_disp_results <- gsub("  ",  " ",sane_disp_results)
  sane_disp_results <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", sane_disp_results, perl=TRUE)
  
  # Print results on plot canvas
  y_pos_res = 1.2
  for (result_name in values$disp.results) {
    text(-2 , y_pos_res, 
         paste(sane_disp_results[which(colnames(object) == result_name)],
               ": ", object[nnumber, result_name][[1]], sep = ""), adj = c(0,0))
    y_pos_res = y_pos_res - 0.2
  }
  
  # Legend
  if(values$v.color != "-Select Entry-") {
    color_var <- get.vertex.attribute(graphs[[nnumber]], values$v.color)
    #color_var[is.na(color_var)] <- 0
    title_ <- ifelse(input$l.label == "", values$v.color, input$l.label)
    legend(x =-2, y = -0.8, legend= levels(factor(color_var)), pt.bg = colors_, pt.cex = 1.5,  pch = 22, bty ="n", y.intersp = 1, title = title_)
  }
}
