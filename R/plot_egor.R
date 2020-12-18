#' Plotting *egor* objects
#' 
#' *egor* objects can be plotted as *egographs* or *egograms*. By
#' default networks of the four first egos are plotted.
#' @param x An _egor_ object.
#' @param ego_no Ego row number.
#' @param x_dim Number of ego networks to plot horizontally.
#' @param y_dim Number of ego networks to plot vertically
#' @param venn_var Name (character) of alter column.
#' @param pie_var Name (character) of alter column.
#' @param vertex_size_var Name (`character`) of alter column.
#' @param vertex_color_var Name (`character`) of alter column.
#' @param vertex_color_palette Name (`character`) of color palette, see details for available color palettes.
#' @param vertex_color_legend_label Character.
#' @param vertex_label_var Name (`character`) of alter column. Set this to `NULL` to suppress labels.
#' @param edge_width_var Name (`character`) of aatie column.
#' @param ego_alter_edge_width_var Name (`character`) of alter column.
#' @param edge_color_var Name (`character`) of aatie column.
#' @param ego_alter_edge_color_var Name (`character`) of alter column.
#' @param edge_color_palette Name (`character`) of color palette, see details for available color palettes.
#' @param highlight_box_col_var Name (`character`) of ego column.
#' @param highlight_box_col_palette Name (`character`) of color palette, see details for available color palettes.
#' @param res_disp_vars Name (`character`) of ego column.
#' @param vertex_zoom Numeric.
#' @param edge_zoom Numeric.
#' @param font_size Numeric.
#' @param venn_colors `Character` vector of colors to be used for coloring the 
#' subsections of the circle.
#' @param venn_gradient_reverse `Logical`, set to TRUE in order to have the color intensity
#' of venns increase going from the inner circles to the outer circles.
#' @param show_venn_labels Logical.
#' @param include_ego Logical.
#' @param type Character. Either "egograph" or "egogram".
# @param layout `Matrix` of x and y coordinates for nodes. Defaults to 
# Fruchterman Rheingold layout algorithm.
#' @param ... Additional arguments forwared to plot.igraph.
#' @details For type eqals "egograph" ego networks are plotted with `igraph`'s
#' plotting engine. "egogram" uses a special layout that places the nodes
#' on a map of (1) concentric circles with (2) subsections, that can be mapped to 
#' alter variables.
#' 
#' Available color palettes are:
#' 
#' - Heat Colors
#' - Yellow-Green
#' - Red-Yellow
#' - Blue-Red
#' - Black-White
#' - Greys
#' - Rainbow
#' - Topo Colors
#' 
#' @examples 
#' e <- make_egor(net.count = 5, max.alters = 12)
#' plot_egograms(x = e,
#'               ego_no = 2,
#'               venn_var = "sex",
#'               pie_var = "country",
#'               vertex_size_var = "age")
#' plot(e)
#' @export
plot_egor <- function(x, ego_no = 1, x_dim = 2, y_dim = 2, ..., type = c("egograph", "egogram")) {
  if (type[1] == "egograph") {
    plot_ego_graphs(x, ego_no = ego_no, x_dim = x_dim, y_dim = y_dim, ...)
  } else if (type[1] == "egogram") {
    plot_egograms(x, ego_no = ego_no, x_dim = x_dim, y_dim = y_dim, ...)
  }
}

#' @rdname plot_egor
#' @method plot egor
#' @export
plot.egor <- function(x, ...) {
  plot_egor(x, ...)
}

