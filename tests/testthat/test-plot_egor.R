context("test-plot_egor.R")

pdf(NULL) # This ensures that no PDF file is gernerated when running tests automatically.

# These tests are mostly here to notify when, other parts of the package break
# the plotting facilities. They are not about the correctness of the plots.

test_that("plot plots egor objects", {
  expect_error({
    e <- make_egor(5, 5)
    
    plot(
      x = e,
      x_dim = 2,
      y_dim = 2,
      ego_no = 1,
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
      ego_no = 1,
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
      ego_no = 1,
      venn_var = "age",
      pie_var = "country",
      venn_colors = c("blue", "lightblue", "mistyrose",
                      "lightcyan"),
      show_venn_labels = TRUE,
      type = "egogram"
    )
  }, NA)
  
})

test_that("plot_egograms works with minimal arguments", {
  expect_error({
    e <- make_egor(net.count = 5, max.alters = 12)
    plot_egograms(x = e,
                  ego_no = 2,
                  venn_var = "sex",
                  pie_var = "country",
                  vertex_size_var = "age")
    plot_egograms(x = e,
                  ego_no = 2,
                  venn_var = "sex",
                  pie_var = "country")
  }, NA)
})

test_that("plot_ego_graphs works with minmal arguments", {
  expect_error({
    e <- make_egor(5, 15)
    plot_ego_graphs(e)
  }, NA)
})

test_that("plot_ego_graphs works with vertex_color_var", {
  expect_error({
    e <- make_egor(15, 15)
    plot_ego_graphs(e, vertex_color_var = "sex")
    plot_ego_graphs(e,
                    vertex_color_var = "sex",
                    vertex_color_legend_label = "Sex")
  }, NA)
})

test_that("plot_egograms doesn't fail on empty alters or aaties", {
  e <- make_egor(5, 5)
  e$aatie <- 
    e$aatie %>% 
    filter(.egoID != 1)
  expect_error(plot_egograms(
    x = e,
    venn_var = "sex",
    pie_var = "country",
    show_venn_labels = TRUE
  ),NA)
  e <- make_egor(5, 5)
  e$alter <- 
    e$alter %>% 
    filter(.egoID != 1)
  expect_error(plot_egograms(
    x = e,
    venn_var = "sex",
    pie_var = "country",
    show_venn_labels = TRUE
  ),NA)
})

test_that("plot_egograms plots with and without venn labels", {
  expect_error({
    e <- make_egor(5, 5)
    plot_egograms(
      e,
      venn_var = "sex",
      pie_var = "country",
      show_venn_labels = FALSE
    )
    plot_egograms(
      e,
      venn_var = "sex",
      pie_var = "country",
      show_venn_labels = TRUE
    )
  }, NA)
})

test_that("plotting works when active data level is not ego",
          {
            expect_error({
              e <- make_egor(5, 15) %>%
                activate(alter)
              
              plot_egograms(
                e,
                1,
                x_dim = 1,
                y_dim = 1,
                "sex",
                "country",
                show_venn_labels = FALSE
              )
              
              plot_ego_graphs(e, 1)
              
              e <- make_egor(5, 15) %>%
                activate(aatie)
              
              plot_egograms(
                e,
                1,
                x_dim = 1,
                y_dim = 1,
                "sex",
                "country",
                show_venn_labels = FALSE
              )
              
              plot_ego_graphs(e, 1)
            }, NA)
          })

test_that("plot_ego_graphs is fast", {
  # This is meant more as a manual test. Plots should appear immediately.
  expect_error({
    plot_ego_graphs(make_egor(12, 16))
    plot_ego_graphs(make_egor(120, 16))
  }, NA)
})

test_that("plot_ego_gram adjusts node size according to venn count", {
  # This is meant more as a manual test. Plots should appear immediately.
  expect_error({
    e <- make_egor(5, 5)
    
    plot_egograms(x = e,
                  ego_no = 2,
                  venn_var = "sex",
                  pie_var = "country")
    plot_egograms(x = e,
                  ego_no = 2,
                  venn_var = "country",
                  pie_var = "age")
    plot_egograms(x = e,
                  ego_no = 2,
                  venn_var = "age",
                  pie_var = "age")
    plot_egograms(x = e,
                  ego_no = 2,
                  venn_var = "age",
                  pie_var = "age",
                  vertex_zoom = 2,
                  edge_zoom = 2)
  }, NA)
})

test_that("plot_ego_gram works with edge arguments", {
  # This is meant more as a manual test. Plots should appear immediately.
  expect_error({
    e <- make_egor(5, 32)
    
    plot_egograms(e,
                  ego_no = 1,
                  venn_var = "sex",
                  pie_var = "country", 
                  vertex_color_var = "sex",
                  edge_color_var = "weight",
                  edge_width_var = "weight", 
                  edge_zoom = 3)
    plot_ego_graphs(
      x = e,
      ego_no = 1,
      edge_color_var = "weight",
      edge_width_var = "weight", 
      edge_zoom = 2
    )
    
  }, NA)
})

dev.off() # Closing the NULL pdf device.