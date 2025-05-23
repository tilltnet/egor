#' @export
#' @describeIn plot_egor Plots an ego graph.
plot_ego_graphs <- function(x,
                            ego_no = 1,
                            x_dim = 1,
                            y_dim = 1,
                            vertex_size_var = NULL,
                            vertex_color_var = NULL,
                            vertex_color_palette = "Heat Colors",
                            vertex_color_legend_label = vertex_color_var,
                            vertex_label_var = "name",
                            edge_width_var = NULL,
                            ego_alter_edge_width_var = 
                              if(!is.null(edge_width_var) & include_ego) edge_width_var,
                            edge_color_var = NULL,
                            ego_alter_edge_color_var = 
                              if(!is.null(edge_color_var) & include_ego) edge_color_var,
                            edge_color_palette = "Heat Colors",
                            highlight_box_col_var = NULL,
                            highlight_box_col_palette = "Heat Colors",
                            res_disp_vars = NULL,
                            vertex_zoom = 1,
                            edge_zoom = 3,
                            font_size = 1,
                            include_ego = FALSE,
                            ...) {
  require_igraph(paste(sQuote("egor"),"plotting ego graphs"))
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow = c(y_dim, x_dim))
  for (i in ego_no:(ego_no + (x_dim * y_dim - 1))) {
    if (i <= nrow(x$ego)) {
      boxi_color <- "#ffffff00"
      if (!is.null(highlight_box_col_var)) {
        var_ <- factor(as_tibble(x$ego)[[highlight_box_col_var]])
        boxi_color <- egor_col_pal(highlight_box_col_palette,
                                   length(levels(var_)))[var_][i]
      }
      plot_one_ego_graph(
        x,
        i,
        vertex_size_var = vertex_size_var,
        vertex_color_var = vertex_color_var,
        vertex_color_palette = vertex_color_palette,
        vertex_color_legend_label = vertex_color_legend_label,
        vertex_label_var = vertex_label_var,
        edge_width_var = edge_width_var,
        ego_alter_edge_width_var = ego_alter_edge_width_var,
        edge_color_var = edge_color_var,
        ego_alter_edge_color_var = ego_alter_edge_color_var,
        edge_color_palette = edge_color_palette,
        highlight_box_col = boxi_color,
        res_disp_vars = res_disp_vars,
        vertex_zoom = vertex_zoom,
        edge_zoom = edge_zoom,
        font_size = font_size,
        include_ego = include_ego,
        ...
      )
    }
  }
}


plot_one_ego_graph <- function(x,
                               ego_no,
                               vertex_size_var = NULL,
                               vertex_color_var = NULL,
                               vertex_color_palette = "Heat Colors",
                               vertex_color_legend_label = vertex_color_var,
                               vertex_label_var = "name",
                               edge_width_var = NULL,
                               ego_alter_edge_width_var = edge_width_var,
                               edge_color_var = NULL,
                               ego_alter_edge_color_var = edge_color_var,
                               edge_color_palette = "Heat Colors",
                               highlight_box_col = "white",
                               res_disp_vars = NULL,
                               vertex_zoom = 1,
                               edge_zoom = 3,
                               font_size = 1,
                               include_ego = FALSE,
                               layout = NULL,
                               ...) {
  x <- 
    slice.egor(activate(x, "ego"), ego_no)
  
  gr <- as_igraph(x, 
                  include.ego = include_ego, 
                  ego.alter.weights = c(ego_alter_edge_width_var,
                                        ego_alter_edge_color_var)
                  )[[1]]
  if (!sum(igraph::V(gr)) > 0) {
    # Plot Error message.
    plot(
      NULL ,
      xaxt = 'n',
      yaxt = 'n',
      bty = 'n',
      ylab = '',
      xlab = '',
      xlim = 0:1,
      ylim = 0:1
    )
    text(0.5, 0.5, 'No alter data\n available for \nthis ego.')
    return()
  }
  
  # Default Colors
  colors_ <- blues9
  e_colors <- "grey69"
  
  # Vertex Size
  if (!is.null(vertex_size_var)) {
    vertex.size <-
      as.numeric(as.factor(igraph::vertex_attr(gr, vertex_size_var)))
    vertex.size[is.na(vertex.size)] <- 0.1
    vertex.size <- vertex.size * vertex_zoom + .1
  } else {
    vertex.size <-
      rep(5, length(igraph::V(gr))) * vertex_zoom
  }
  
  # Vertex Color
  if (!is.null(vertex_color_var)) {
    vertex.color <-
      igraph::vertex_attr(gr, vertex_color_var)
    #vertex.color[is.na(vertex.color)] <- 0
    vertex.color <- factor(vertex.color)
    colors_ <- egor_col_pal(vertex_color_palette,
                            length(levels(
                              factor(igraph::vertex_attr(gr,
                                                                  vertex_color_var))
                            )))
    clrs <- colors_[vertex.color]
    clrs[is.na(clrs)] <- "#ffffff"
  } else {
    vertex.color <- 1
    clrs <- "coral"
  }
  
  # Edge Width
  if (!is.null(edge_width_var)) {
    edge.width <-
      igraph::edge_attr(gr, edge_width_var) * edge_zoom
    #edge.width[is.na(edge.width)] <- 0
  } else {
    edge.width <-
      rep(1, length(igraph::E(gr))) * edge_zoom
  }
  
  # Edge Color
  if (!is.null(edge_color_var)) {
    edge.color <- igraph::edge_attr(gr, edge_color_var)
    #edge.color[is.na(edge.color)] <- 0
    edge.color <- as.numeric(factor(edge.color))
    e_colors <- egor_col_pal(edge_color_palette,
                             length(levels(
                               factor(igraph::edge_attr(gr,
                                                                 edge_color_var))
                             )))
  } else {
    edge.color <- 1
  }

    # Label
  if (!is.null(vertex_label_var)) {
    vertex.label <-
      igraph::vertex_attr(gr, vertex_label_var)
    vertex.label[is.na(vertex.label)] <- 0
  } else {
    vertex.label <- ""
  }
  
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  if (!is.null(vertex_color_var))
    par(mar = c(0.5, 5, 0.5, 0.5))
  
  if (is.null(layout)) {
    layout_ <-
      igraph::layout_with_fr(gr, weights = edge.width)
  } else {
    layout_ <- layout
  }

  if(include_ego) {
    # Set curvature of ego-alter ties to zero
    # igraph::E(gr)$curved[is.na(igraph::E(gr)$curved)] <- 0
    # Set ego-alter weights to a dummy value
    if (any(!is.na(igraph::E(gr)$weight))) {
      # Set to min of other weights, so scale of weights is comparable
      igraph::E(gr)$weight[is.na(igraph::E(gr)$weight)] <- min(igraph::E(gr)$weight, na.rm = TRUE)
    } else {
      # no other weights in the graph, so just set a hardwired dummy value
      # if there is no weight variable at all, E(gr)$weight will be NULL rather than a vector,
      # so the syntax igraph::E(gr)$weight[is.na(igraph::E(gr)$weight)] will fail. Since all weights
      # are NULL or NA at this point, set them all to 1. This will change missing aatie weights to 1,
      # which may not be desirable, but overwriting missing aatie weights is also the behavior of the
      # code above when there is at least one nonmissing aatie weight
      igraph::E(gr)$weight <- 1
    }
  }
  
  igraph::plot.igraph(
    gr,
    vertex.size = vertex.size,
    vertex.color = clrs,
    edge.width = edge.width,
    vertex.label = vertex.label,
    edge.color = e_colors[edge.color],
    layout = layout_,
    vertex.label.cex = font_size,
    vertex.label.family = "sans",
    ...
  )
  # Sanitize Variable Names
  sane_disp_results <- gsub("\\.",  " ", names(x$ego))
  sane_disp_results <- gsub("  ",  " ", sane_disp_results)
  sane_disp_results <-
    gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", sane_disp_results, perl = TRUE)
  
  # Print results on plot canvas
  y_pos_res = -1.1
  for (result_name in res_disp_vars) {
    text(
      -1.8 ,
      y_pos_res,
      paste(sane_disp_results[which(colnames(x$ego) == result_name)],
            ": ", as_tibble(x$ego)[[result_name]][[1]], sep = ""),
      adj = c(0, 0),
      cex = font_size
    )
    y_pos_res = y_pos_res + 0.2
  }
  
  # Legend
  
  if (!is.null(vertex_color_var)) {
    color_var <- igraph::vertex_attr(gr, vertex_color_var)
    #color_var[is.na(color_var)] <- 0
    title_ <-
      ifelse(vertex_color_legend_label == "",
             vertex_color_var,
             vertex_color_legend_label)
    legend(
      x = -1.9,
      y = 1.1,
      legend = levels(factor(color_var)),
      pt.bg = colors_,
      pt.cex = 1.5,
      pch = 22,
      bty = "n",
      y.intersp = 1,
      title = title_,
      xpd = TRUE,
      cex = font_size
    )
  }
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  graphics::box(lty = 'solid', col = highlight_box_col, lwd = 5)
}
