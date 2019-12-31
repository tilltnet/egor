context("test-plot_egor.R")

test_that("plot plots egor objects", {
  e <- make_egor(5, 5)
  
  plot(
    x = e,
    x_dim = 2,
    y_dim = 2,
    nnumber = 1,
    vertex_size_var = "age.years",
    vertex_color_var = "age.years",
    vertex_color_palette = "Greys",
    vertex_color_legend_label = "Age",
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
    res_disp_vars  = c("sex", "age"))
  
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

})



test_that("plot_egograms works with minmal arguments", {
  e <- make_egor(5, 5)
  plot_egograms(e,
                venn_var = "sex",
                pie_var = "country")
})

test_that("plot_ego_graphs works with minmal arguments", {
  e <- make_egor(5, 15)
  plot_ego_graphs(e)
})

test_that("plot_egograms plots with and without venn labels", {
  e <- make_egor(5, 5)
  plot_egograms(e,
                venn_var = "sex",
                pie_var = "country", show_venn_labels = FALSE)
})

test_that("plotting works when active data level is not ego",
          {
            e <- make_egor(5, 15) %>% 
              activate(alter)
            
            plot_egograms(e,
                          1,
                          x_dim = 1,
                          y_dim = 1,
                          "sex",
                          "country",
                          show_venn_labels = FALSE)
            
            plot_ego_graphs(e, 1)
            
            e <- make_egor(5, 15) %>% 
              activate(aatie)
            
            plot_egograms(e,
                          1,
                          x_dim = 1,
                          y_dim = 1,
                          "sex",
                          "country",
                          show_venn_labels = FALSE)
            
            plot_ego_graphs(e, 1)
            
          })
