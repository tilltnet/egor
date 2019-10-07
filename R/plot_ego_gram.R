if (getRversion() >= "2.15.1") utils::globalVariables(c(".egoID"))

calc_angle_coordinates <- function(radius, angle) {
  X <- radius * sin(angle)
  Y <- radius * cos(angle)
  c(X, Y)
}

calc_angle_coordinates(0.2, 1)

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
  venn_grid_df %>% arrange(.altID)
}


#' @export
#' @describeIn plot_egor Plots an ego-socio-gram.
plot_egograms <- function(x,
                          nnumber = 1,
                          x_dim = 1,
                          y_dim = 1,
                          venn_var,
                          pie_var,
                          vertex_size_var = NULL,
                          vertex_color_var = NULL,
                          vertex_color_palette = "Heat Colors",
                          vertex_color_legend_label = NULL,
                          vertex_label_var = NULL,
                          edge_width_var = NULL,
                          edge_color_var = NULL,
                          edge_color_palette = "Heat Colors",
                          highlight_box_col_var = NULL,
                          highlight_box_col_palette = "Heat Colors",
                          res_disp_vars = NULL,
                          vertex_zoom = 1,
                          edge_zoom = 3,
                          font_size = 1,
                          venn_colors = NULL,
                          show_venn_labels = TRUE,
                          ...) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow = c(y_dim, x_dim))
  
  for (i in nnumber:(nnumber + (x_dim * y_dim - 1))) {
    if (i <= nrow(x$ego)) {
      boxi_color <- "white"
      if (!is.null(highlight_box_col_var)) {
        var_ <- factor(x$ego[[highlight_box_col_var]])
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
        font_size = font_size,
        ...
      )
    }
  }
}

plot_egogram <- function(x, nnumber, venn_var, pie_var,
                         vertex_size_var = NULL,
                         vertex_color_var = NULL,
                         vertex_color_palette = "Heat Colors",
                         vertex_color_legend_label = NULL,
                         vertex_label_var = NULL,
                         edge_width_var = NULL,
                         edge_color_var = NULL,
                         edge_color_palette = "Heat Colors",
                         highlight_box_col = NULL,
                         highlight_box_col_palette = "Heat Colors",
                         res_disp_vars = NULL,
                         vertex_zoom = 1,
                         edge_zoom = 3,
                         font_size = 1,
                         venn_colors = NULL,
                         show_venn_labels = TRUE,
                         ...)  {
  par(mar = c(1,0.5,0.5,0.5))
  
  ego_object <-
    slice(x, nnumber)
  
  venn_var <- ego_object$alter[[venn_var]]
  pie_var <- ego_object$alter[[pie_var]]
  
  if (is.numeric(venn_var)) {
    venn_var <- factor(venn_var, levels = min(venn_var):max(venn_var))
  }
  
  if (is.numeric(pie_var)) {
    pie_var <- factor(pie_var, levels = min(pie_var):max(pie_var))
  }
  
  if (is.character(venn_var)) {
    venn_var <- factor(venn_var, levels =unique(x$alter[[venn_var]]))
  }
  
  if (is.character(pie_var)) {
    pie_var <- factor(pie_var, levels = unique(x$alter[[pie_var]]))
  }
  
  venn_n <- length(levels(venn_var))
  piece_n <- length(levels(pie_var))
  min_dist <- 1/(venn_n + 1)
  
  # Pieces of the pie
  plot.new()
  pie(rep(1, piece_n),
      labels =   pie_labels <- levels(pie_var),
      radius = 1, 
      clockwise = TRUE, 
      border = FALSE, 
      add = TRUE,
      col = venn_colors)
  
  
  # Venns
  theta <- seq(0, 2 * pi, length = 200)
  i <- 0
  for (radius in c(1:(venn_n + 1) / (venn_n + 1))) {
    graphics::lines(x = radius * cos(theta), y = radius * sin(theta), col = "grey")
    # Venn Labels
    if (i > 0 & show_venn_labels) {
      graphics::lines(c(0,1.8), c(radius,radius), col = "grey80", lty = "dashed")
      text(1.3,radius - 0.05, levels(venn_var)[i], cex = 0.8)
    }
    i <- i + 1
  }
    
  # Block inner cicle
  pie(
    1,
    labels = NA,
    border = FALSE,
    add = TRUE,
    radius = 1 / (venn_n + 1) + 0.01
  )
  
  # Layout
  lay <- layout_egogram(
    altID = ego_object$alter$.altID,
    venn_var = (venn_var),
    pie_var = (pie_var)
  )
  
  # Join Layout and Calculate Distances
  if (activate(x, "aatie") %>% 
     as_tibble() %>% 
     nrow() > 0) {
    
    
    lay$.altID <- as.character(lay$.altID)
    
    a <- 
      activate(x, "aatie") %>% 
      as_tibble() %>%
      mutate(.srcID = as.character(.srcID),
           .tgtID = as.character(.tgtID)) %>% 
    
      left_join(lay, by = c(".srcID" = ".altID")) %>% 
      left_join(lay, by = c(".tgtID" = ".altID"))
    b <- a %>% 
      group_by(.egoID, .srcID, .tgtID) %>% 
      do({data.frame(x = c(.$x.x, .$x.y), y = c(.$y.x, .$y.y)) %>% 
          stats::dist() %>%  .[[1]] %>% tibble(distance = .) %>% 
          mutate(curved = case_when(
            distance <= min_dist ~ 1,
            TRUE ~ 0.1
          ))})
    ego_object$aatie <- b
  }
  
  # Create igraph
  g <- as_igraph(ego_object)
  
  # Plot
  plot(
    g[[1]],
    layout = lay[1:2] %>% as.matrix(),
    add = TRUE,
    rescale = FALSE,
    vertex.size = min_dist * 50,
    vertex.frame.color = NA,
    edge.curved = E(g[[1]])$curved,
    edge.width = 2,
    edge.color = "gray69", #rgb(0,0,0,0.2)
    ...
  )
}


pie <- function(x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
                 init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                 col = NULL, border = NULL, lty = NULL, main = NULL, add = FALSE, ...) 
{
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- grDevices::as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  if (!add)
    plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush())
  graphics::plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
     c("white", "lightblue", "mistyrose", 
        "lightcyan", "lavender", "cornsilk")
  else par("fg")
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
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    graphics::polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      graphics::lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  graphics::title(main = main, ...)
  invisible(NULL)
}
