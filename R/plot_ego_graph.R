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
                            ego_color_var = vertex_color_var,
                            ego_color_palette = vertex_color_palette,
                            ego_color_legend_label = ego_color_var,
                            ego_label_var = vertex_label_var,
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
                            ego_attrs = NULL,
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
        ego_attrs = ego_attrs,
        ego_color_var = ego_color_var,
        ego_color_palette = ego_color_palette,
        ego_color_legend_label = ego_color_legend_label,
        ego_label_var = ego_label_var,
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
                               ego_color_var = vertex_color_var,
                               ego_color_palette = vertex_color_palette,
                               ego_color_legend_label = ego_color_var,
                               ego_label_var = vertex_label_var,
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
                               ego_attrs = NULL,
                               layout = NULL,
                               ...) {
  x <- 
    slice.egor(activate(x, "ego"), ego_no)
  
  if (include_ego) {
    if (vertex_label_var %in% names(x$ego)) {
      ego_attrs <- c(ego_attrs, vertex_label_var)
    }
    # Add ego_label_var to ego_attrs if it's different from vertex_label_var
    if (!is.null(ego_label_var) && 
        ego_label_var %in% names(x$ego) && 
        !identical(ego_label_var, vertex_label_var)) {
      ego_attrs <- c(ego_attrs, ego_label_var)
    }
    if (!is.null(vertex_color_var) && vertex_color_var %in% names(x$ego)) {
      ego_attrs <- c(ego_attrs, vertex_color_var)
    }
    # Add ego_color_var to ego_attrs if it's different from vertex_color_var
    ego_needs_separate_color_var <- !is.null(ego_color_var) && 
                                     ego_color_var %in% names(x$ego) && 
                                     !identical(ego_color_var, vertex_color_var)
    if (ego_needs_separate_color_var) {
      ego_attrs <- c(ego_attrs, ego_color_var)
    }
  }
  
  gr <- as_igraph(x, 
                  include.ego = include_ego, 
                  ego.attrs = ego_attrs,
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
                            length(levels(vertex.color)))
    clrs <- colors_[vertex.color]
    clrs[is.na(clrs)] <- "#ffffff"
  } else {
    vertex.color <- 1
    clrs <- "coral"
  }
  
  # Ego Color (if include_ego is TRUE and ego_color_var is specified)
  # Note: When include_ego=TRUE, the ego vertex is always added as the last vertex in the igraph
  if (include_ego && !is.null(ego_color_var)) {
    # Determine if ego needs separate coloring
    ego_has_diff_color_config <- !identical(ego_color_var, vertex_color_var) || 
                                  !identical(ego_color_palette, vertex_color_palette)
    
    if (ego_has_diff_color_config) {
      # If ego_color_var and vertex_color_var are the same variable but different palettes
      if (identical(ego_color_var, vertex_color_var)) {
        # Same variable, different palette - use the combined levels but apply ego_color_palette to ego
        ego_colors_ <- egor_col_pal(ego_color_palette,
                                    length(levels(vertex.color)))
        # The last vertex is always ego when include_ego=TRUE
        clrs[length(clrs)] <- ego_colors_[vertex.color[length(vertex.color)]]
      } else {
        # Different variables - ego_color_var should be an ego-level attribute
        # Check if the attribute exists in the graph (it should for the ego vertex)
        if (ego_color_var %in% igraph::vertex_attr_names(gr)) {
          ego_color_values <- igraph::vertex_attr(gr, ego_color_var)
          # Get unique non-NA values to determine factor levels
          unique_ego_values <- unique(ego_color_values[!is.na(ego_color_values)])
          ego_color_factor <- factor(ego_color_values, levels = unique_ego_values)
          ego_colors_ <- egor_col_pal(ego_color_palette,
                                      length(levels(ego_color_factor)))
          # Apply ego color only to the last vertex (ego)
          ego_idx <- length(clrs)
          if (!is.na(ego_color_factor[ego_idx])) {
            clrs[ego_idx] <- ego_colors_[ego_color_factor[ego_idx]]
          } else {
            clrs[ego_idx] <- "#ffffff"
          }
        }
      }
    }
    # If ego_color_var and vertex_color_var are identical (including palette),
    # the ego color is already set correctly by the vertex color logic above
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
    if(include_ego && is.na(vertex.label[length(vertex.label)])) {
      vertex.label[length(vertex.label)] <- "ego"
    }
    vertex.label[is.na(vertex.label)] <- 0
  } else {
    vertex.label <- ""
  }
  
  # Ego Label (if include_ego is TRUE and ego_label_var is different)
  if (include_ego && !is.null(ego_label_var) && !identical(ego_label_var, vertex_label_var)) {
    # Check if the ego_label_var attribute exists in the graph
    if (ego_label_var %in% igraph::vertex_attr_names(gr)) {
      ego_label_value <- igraph::vertex_attr(gr, ego_label_var)[length(igraph::V(gr))]
      # Only set if not NA
      if (!is.na(ego_label_value)) {
        vertex.label[length(vertex.label)] <- ego_label_value
      }
    }
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
    # Only set edge attributes if there are edges
    if (igraph::ecount(gr) > 0) {
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
    
    # Determine if we need a separate ego legend
    # Reuse the same condition as in ego color logic
    ego_has_diff_color_config <- include_ego && !is.null(ego_color_var) && 
                                 (!identical(ego_color_var, vertex_color_var) || 
                                  !identical(ego_color_palette, vertex_color_palette))
    
    if (ego_has_diff_color_config && !identical(ego_color_var, vertex_color_var)) {
      # Different variables: show two legends stacked
      # Constants for legend spacing
      LEGEND_ITEM_HEIGHT <- 0.15
      LEGEND_VERTICAL_GAP <- 0.3
      
      # First show alter/vertex legend
      # Note: ego is always the last vertex when include_ego=TRUE
      alter_color_var <- color_var[-length(color_var)]  # Exclude ego
      legend(
        x = -1.9,
        y = 1.1,
        legend = levels(factor(alter_color_var)),
        pt.bg = colors_,
        pt.cex = 1.5,
        pch = 22,
        bty = "n",
        y.intersp = 1,
        title = paste0("Alter: ", title_),
        xpd = TRUE,
        cex = font_size
      )
      
      # Now show ego legend below
      ego_color_var_values <- igraph::vertex_attr(gr, ego_color_var)
      ego_color_factor <- factor(ego_color_var_values)
      ego_colors_ <- egor_col_pal(ego_color_palette,
                                  length(levels(ego_color_factor)))
      ego_title_ <-
        ifelse(ego_color_legend_label == "",
               ego_color_var,
               ego_color_legend_label)
      
      # Calculate y position for second legend
      n_alter_levels <- length(levels(factor(alter_color_var)))
      y_offset <- 1.1 - (n_alter_levels * LEGEND_ITEM_HEIGHT) - LEGEND_VERTICAL_GAP
      
      legend(
        x = -1.9,
        y = y_offset,
        legend = levels(ego_color_factor),
        pt.bg = ego_colors_,
        pt.cex = 1.5,
        pch = 22,
        bty = "n",
        y.intersp = 1,
        title = paste0("Ego: ", ego_title_),
        xpd = TRUE,
        cex = font_size
      )
    } else {
      # Same variable or same variable with different palette: show single legend
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
  }
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  graphics::box(lty = 'solid', col = highlight_box_col, lwd = 5)
}
