context("test_vis_app.R")



if (FALSE) {
  library(shiny)
  library(egor)
  f <- make_egor(50, 50)
  ab <- filter(e, sex == "w")
  egor_vis_app()
}

e <- make_egor(5, 5)

plot(
  x = e,
  x_dim = 2,
  y_dim = 2,
  nnumber = 1,
  vertex_size_var = "age.years",
  vertex_color_var = "age.years",
  vertex_color_palette = "Greys",
  vertex_color_legend_label = "Mushi",
  edge_width_var = "weight",
  edge_color_var = "weight",
  edge_color_palette = "Greys",
  highlight_box_col_var = "blue",
  res_disp_vars  = c("sex", "age")
)

plot(
  x = e,
  x_dim = 2,
  y_dim = 2,
  nnumber = 1,
  vertex_size_var = "age.years",
  vertex_color_var = "age.years",
  vertex_color_palette = "Greys",
  vertex_color_legend_label = "Mushi",
  edge_width_var = "weight",
  edge_color_var = "weight",
  edge_color_palette = "Greys",
  highlight_box_col_var = "sex",
  res_disp_vars  = c("sex", "age")
)

plot(
  x = e,
  nnumber = 1,
  venn_var = "age",
  pie_var = "country",
  venn_colors = c("white", "lightblue", "mistyrose",
                  "lightcyan"),
  show_venn_labels = TRUE,
  type = "egogram"
)

plot_egograms(e,
             1,
             x_dim = 1,
             y_dim = 1,
             "sex",
             "country",
             vertex.label = NA,
             show_venn_labels = FALSE)
