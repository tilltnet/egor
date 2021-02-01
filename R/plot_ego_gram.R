if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".egoID"))

calc_angle_coordinates <- function(radius, angle) {
  X <- radius * sin(angle)
  Y <- radius * cos(angle)
  c(X, Y)
}

plot_n_dots_on_arc <- function(radius, start_deg, end_deg, n) {
  #plot(c(-1, 1), c(-1, 1), type = "n", asp = 1)
  n <- n + 2
  arg_length <- end_deg - start_deg
  point_distance <- arg_length / (n - 1)
  for (i in 1:n) {
    if (!i %in% c(1, n)) {
      p <- start_deg + point_distance * (i - 1)
      z <- calc_angle_coordinates(radius, p * (pi / 180))
      graphics::points(z[1], z[2], cex = 4)
    }
  }
}

layout_n_dots_on_arc <- function(radius, start_deg, end_deg, n) {
  #plot(c(-1, 1), c(-1, 1), type = "n", asp = 1)
  n <- n + 2
  arg_length <- end_deg - start_deg
  point_distance <- arg_length / (n - 1)
  arc_grid_df <- tibble()
  for (i in 1:n) {
    if (!i %in% c(1, n)) {
      p <- start_deg + point_distance * (i - 1)
      z <- calc_angle_coordinates(radius, p * (pi / 180))
      arc_grid_df <- rbind(arc_grid_df,
                           tibble(x = z[1], y = z[2]))
    }
  }
  arc_grid_df
}

#' Create layout for an egogram
#' 
#' This creates pairs of x and y coordinates for a egogram, accompanied by 
#' alter IDs for each coordinate pair.
#' @param altID Vector of alter IDs.
#' @param venn_var Vector of values representing alter groups corresponding with
#' venns in an egogram.
#' @param pie_var Vector of values representing alter groups corresponding with
#' pieces of pie in an egogram.
#' @return A dataframe with three columns: x, y and altID.
#' @export
layout_egogram <- function(altID, venn_var, pie_var) {
  altID <- factor(altID)
  venn_n <- length(levels(venn_var))
  piece_n <- length(levels(pie_var))
  
  venn_var <- as.numeric(venn_var)
  pie_var <- as.numeric(pie_var)
  
  venn_grid_df <- tibble()
  sign = -1
  for (venn in 1:(venn_n + 1)) {
    sign <-  sign * -1
    for (piece in 1:piece_n) {
      offset <- sample(5:10, 1) * sign
      piece_subset <- pie_var[venn_var == venn]
      altid_subset <- altID[venn_var == venn]
      altid_subset <- altid_subset[piece_subset == piece]
      piece_subset <- piece_subset[piece_subset == piece]
      
      distance <- 360 / piece_n
      start_deg <- (piece - 1) * distance + offset
      end_deg <- start_deg + distance
      venn_grid_df <-
        rbind(venn_grid_df,
              cbind(
                layout_n_dots_on_arc(
                  1 / (venn_n + 1) * (venn + 0.5),
                  start_deg,
                  end_deg,
                  length(piece_subset)
                ),
                .altID = altid_subset
              ))
    }
  }
  arrange(venn_grid_df, .altID)
}


#' @export
#' @describeIn plot_egor Plots an ego-socio-gram.
plot_egograms <- function(x,
                          ego_no = 1,
                          x_dim = 1,
                          y_dim = 1,
                          venn_var = NULL,
                          pie_var = NULL,
                          vertex_size_var = NULL,
                          vertex_color_var = NULL,
                          vertex_color_palette = "Heat Colors",
                          vertex_color_legend_label = vertex_color_var,
                          vertex_label_var = "name",
                          edge_width_var = NULL,
                          edge_color_var = NULL,
                          edge_color_palette = "Heat Colors",
                          highlight_box_col_var = NULL,
                          highlight_box_col_palette = "Heat Colors",
                          res_disp_vars = NULL,
                          vertex_zoom = 1,
                          edge_zoom = 2,
                          font_size = 1,
                          venn_colors = NULL,
                          venn_gradient_reverse = FALSE,
                          show_venn_labels = TRUE,
                          ...) {
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
      plot_egogram(
        x,
        i,
        venn_var,
        pie_var,
        vertex_size_var = vertex_size_var,
        vertex_color_var = vertex_color_var,
        vertex_color_palette = vertex_color_palette,
        vertex_color_legend_label = vertex_color_legend_label,
        vertex_label_var = vertex_label_var,
        edge_width_var = edge_width_var,
        edge_color_var = edge_color_var,
        edge_color_palette = edge_color_palette,
        highlight_box_col = boxi_color,
        res_disp_vars = res_disp_vars,
        vertex_zoom = vertex_zoom,
        edge_zoom = edge_zoom,
        venn_colors = venn_colors,
        venn_gradient_reverse = venn_gradient_reverse,
        font_size = font_size,
        show_venn_labels = show_venn_labels,
        ...
      )
    }
  }
}

plot_egogram <-
  function(x,
           ego_no,
           venn_var = NULL,
           pie_var = NULL,
           vertex_size_var = NULL,
           vertex_color_var = NULL,
           vertex_color_palette = "Heat Colors",
           vertex_color_legend_label = vertex_color_var,
           vertex_label_var = "name",
           edge_width_var = NULL,
           edge_color_var = NULL,
           edge_color_palette = "Heat Colors",
           highlight_box_col = NULL,
           highlight_box_col_palette = "Heat Colors",
           res_disp_vars = NULL,
           vertex_zoom = 1,
           edge_zoom = 2,
           font_size = 1,
           venn_colors = NULL,
           venn_gradient_reverse = FALSE,
           show_venn_labels = TRUE,
           ...)  {
    
    if (!any(c(!is.null(pie_var), !is.null(venn_var))))
      warning("pie_var and venn_var are both NULL. In order to better utilize the plot_egogram() function define at least one of each.")
  
    par(mar = c(1, 0.5, 0.5, 0.5))
    
    # TODO: stop(/warn?) when pie or venn var have more than 10 levels
    
    ego_object <-
      slice(.data = activate(x, "ego"), ego_no)
    
    if (is.null(pie_var)) {
      ego_object$alter$.pie_dummy <- factor(" ")
      pie_var <- ".pie_dummy"
    }
    
    if (is.null(venn_var)) {
      ego_object$alter$.venn_dummy <- factor(" ")
      venn_var <- ".venn_dummy"
    }
    
    pie_var_name <- pie_var
    venn_var_name <- venn_var
    
    venn_var <- ego_object$alter[[venn_var_name]]
    pie_var <- ego_object$alter[[pie_var_name]]
    
    if (is.numeric(venn_var)) {
      venn_var <- factor(venn_var, levels = min(x$alter[[venn_var_name]]):max(x$alter[[venn_var_name]]))
    }
    
    if (is.numeric(pie_var)) {
      pie_var <- factor(pie_var, levels = min(x$alter[[pie_var_name]]):max(x$alter[[pie_var_name]]))
    }
    
    if (is.character(venn_var)) {
      venn_var <- factor(venn_var, levels = unique(x$alter[[venn_var_name]]))
    }
    
    if (is.character(pie_var)) {
      pie_var <- factor(pie_var, levels = unique(x$alter[[pie_var_name]]))
    }
    
    venn_n <- length(levels(venn_var))
    piece_n <- length(levels(pie_var))
    min_dist <- 1 / (venn_n + 1)
    
    # Pieces of the pie
    plot.new()
    pie_add(
      rep(1, piece_n),
      labels = levels(pie_var),
      radius = 1,
      clockwise = TRUE,
      border = FALSE,
      add = TRUE,
      col = venn_colors
    )
    
    # Venns
    radi <- c(1:(venn_n + 1) / (venn_n + 1))
    cols <- paste0("#ffffff", as.hexmode(round(seq(0, 220,  220 / venn_n))))
    if(venn_gradient_reverse) cols <- rev(cols)
    for(i in 1:venn_n) {
      ring(0, 0, radi[i+1], radi[i], col = cols[i], border = "grey70")
    }

    # plotrix::draw.circle(0, 0, c(1:(venn_n + 1) / (venn_n + 1)),
    #                      border = "grey70",
    #                      col = paste0("#ffffff", as.hexmode(seq(0, 140,  140 / venn_n))))
    
    theta <- seq(0, 2 * pi, length = 200)
    i <- 0
    for (radius in c(1:(venn_n + 1) / (venn_n + 1))) {
      # graphics::lines(x = radius * cos(theta),
      #                 y = radius * sin(theta),
      #                 col = "grey")
      # Venn Labels
      if (i > 0 & show_venn_labels) {
        graphics::lines(c(0, 1.8),
                        c(radius, radius),
                        col = "grey80",
                        lty = "dashed")
        text(1.3, radius - 0.05, levels(venn_var)[i], cex = 0.8)
      }
      i <- i + 1
    }

    # Block inner cicle
    pie_add(
      1,
      labels = NA,
      border = FALSE,
      add = TRUE,
      radius = 1 / (venn_n + 1) + 0.01,
      col = "white"
    )
    
    # Layout
    lay <- layout_egogram(
      altID = ego_object$alter$.altID,
      venn_var = (venn_var),
      pie_var = (pie_var)
    )
    
    # Join Layout and Calculate Distances
    if (nrow(as_tibble(activate(ego_object, "aatie"))) > 0) {
      lay$.altID <- as.character(lay$.altID)
      
      # get additional edge variable names
      additional_edge_vars <- names(ego_object$aatie)
      additional_edge_vars <-
        additional_edge_vars[!additional_edge_vars %in% c(".egoID",
                                                          ".srcID",
                                                          ".tgtID")]
      
      a <-
        as_tibble(activate(ego_object, "aatie"))
      
      a <- mutate(a, .srcID = as.character(.srcID),
               .tgtID = as.character(.tgtID))
        
      a <- left_join(a, lay, by = c(".srcID" = ".altID"))
      a <- left_join(a, lay, by = c(".tgtID" = ".altID"))
      b <- 
        
        do(group_by(a, .egoID, .srcID, .tgtID), {
          dist_curved_df <- 
            data.frame(x = c(.$x.x, .$x.y), y = c(.$y.x, .$y.y))
          dist_curved_df <- 
            tibble(distance = stats::dist(dist_curved_df)[[1]])
          dist_curved_df <- 
            mutate(dist_curved_df, 
                   curved = case_when(distance <= min_dist ~ 1,
                                      TRUE ~ 0.1))
          cbind(dist_curved_df, .[additional_edge_vars])
        })
      ego_object$aatie <- b
    }
    
    vertex_zoom <- (((venn_n+5)^2-1)/(venn_n+5)^3) * 100 - 20 + vertex_zoom 

    # Create igraph
    g <- as_igraph(ego_object)
    
    # Plot
    plot_one_ego_graph(
      ego_object,
      ego_no = 1,
      layout = as.matrix(lay[1:2]),
      add = TRUE,
      rescale = FALSE,
      vertex_size_var = vertex_size_var,
      vertex_color_var = vertex_color_var,
      vertex_color_palette = vertex_color_palette,
      vertex_color_legend_label = vertex_color_legend_label,
      vertex_label_var = vertex_label_var,
      edge_width_var = edge_width_var,
      edge_color_var = edge_color_var,
      edge_color_palette = edge_color_palette,
      highlight_box_col = highlight_box_col,
      res_disp_vars = res_disp_vars,
      vertex_zoom = vertex_zoom,
      edge_zoom = edge_zoom,
      vertex.frame.color = NA,
      edge.curved = igraph::E(g[[1]])$curved,
      #edge.width = 2,
      #edge.color = "gray69",
      #rgb(0,0,0,0.2),
      ...
    )
  }

# This is the graphics::pie function modified to allow adding its
# output to an existing plot
pie_add <- function(x,
                labels = names(x),
                edges = 200,
                radius = 0.8,
                clockwise = FALSE,
                init.angle = if (clockwise)
                  90
                else
                  0,
                density = NULL,
                angle = 45,
                col = NULL,
                border = NULL,
                lty = NULL,
                main = NULL,
                add = FALSE,
                ...)
{
  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive.")
  if (is.null(labels))
    labels <- as.character(seq_along(x))
  else
    labels <- grDevices::as.graphicsAnnot(labels)
  x <- c(0, cumsum(x) / sum(x))
  dx <- diff(x)
  nx <- length(dx)
  if (!add)
    plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L])
    xlim <- (pin[1L] / pin[2L]) * xlim
  else
    ylim <- (pin[2L] / pin[1L]) * ylim
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush())
  graphics::plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col))
    col <- if (is.null(density))
      c("beige",
        "lightblue",
        "mistyrose",
        "lightcyan",
        "lavender",
        "cornsilk")
  else
    par("fg")
  if (!is.null(col))
    col <- rep_len(col, nx)
  if (!is.null(border))
    border <- rep_len(border, nx)
  if (!is.null(lty))
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    density <- rep_len(density, nx)
  twopi <- if (clockwise)
    - 2 * pi
  else
    2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi / 180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    graphics::polygon(
      c(P$x, 0),
      c(P$y, 0),
      density = density[i],
      angle = angle[i],
      border = border[i],
      col = col[i],
      lty = lty[i]
    )
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      graphics::lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(1.1 * P$x,
           1.1 * P$y,
           labels[i],
           xpd = TRUE,
           adj = ifelse(P$x < 0, 1, 0),
           ...)
    }
  }
  graphics::title(main = main, ...)
  invisible(NULL)
}

# https://stackoverflow.com/a/26795448
ring <- function(x,y,outer,inner, border=NULL, col=NA, lty=par("lty"), N=100, ...) {
  part_pi <- pi
  t <- seq(0, part_pi, length.out=N)
  #tx <- seq(0-part_pi/10, part_pi+part_pi/10, length.out=N)
  top <- cbind(c(x+cos(t)*outer, x-cos(t)*inner), c(y+sin(t)*outer, y+sin(t)*inner))
  bot <- cbind(c(x-cos(t)*outer, x+cos(t)*inner), c(y-sin(t)*outer, y-sin(t)*inner))
  out <- cbind(c(x+cos(t)*outer,x-cos(t)*outer),  c(y+sin(t)*outer, y-sin(t)*outer))
  inn <- cbind(c(x-cos(t)*inner, x+cos(t)*inner), c(y+sin(t)*inner,  y-sin(t)*inner))
  if (!is.na(col)) {
    graphics::polygon(top, border=NA, col = col, ...)
    graphics::polygon(bot, border=NA, col = col, ...)
  }
  if(!is.null(border)) {
    graphics::lines(out, col=border, lty=lty)
    graphics::lines(inn, col=border, lty=lty)
  } else {
    graphics::lines(out, lty=lty)
    graphics::lines(inn, lty=lty)     
  }
}
