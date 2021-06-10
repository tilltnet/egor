context("test-plot_egor.R")

pdf(NULL) # This ensures that no PDF file is generated when running tests automatically.

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
      highlight_box_col_var = "sex",
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
    plot_egograms(
      x = e,
      ego_no = 2,
      venn_var = "sex",
      pie_var = "country",
      vertex_size_var = "age"
    )
    plot_egograms(
      x = e,
      ego_no = 2,
      venn_var = "sex",
      pie_var = "country"
    )
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
  ),
  NA)
  e <- make_egor(5, 5)
  e$alter <-
    e$alter %>%
    filter(.egoID != 1)
  expect_error(plot_egograms(
    x = e,
    venn_var = "sex",
    pie_var = "country",
    show_venn_labels = TRUE
  ),
  NA)
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
    
    plot_egograms(
      x = e,
      ego_no = 2,
      venn_var = "sex",
      pie_var = "country"
    )
    plot_egograms(
      x = e,
      ego_no = 2,
      venn_var = "country",
      pie_var = "age"
    )
    plot_egograms(
      x = e,
      ego_no = 2,
      venn_var = "age",
      pie_var = "age"
    )
    plot_egograms(
      x = e,
      ego_no = 2,
      venn_var = "age",
      pie_var = "age",
      vertex_zoom = 2,
      edge_zoom = 2
    )
  }, NA)
})

test_that("plot_ego_gram works with edge arguments", {
  # This is meant more as a manual test. Plots should appear immediately.
  expect_error({
    e <- make_egor(5, 32)
    
    plot_egograms(
      e,
      ego_no = 1,
      venn_var = "sex",
      pie_var = "country",
      vertex_color_var = "sex",
      edge_color_var = "weight",
      edge_width_var = "weight",
      edge_zoom = 3
    )
    plot_ego_graphs(
      x = e,
      ego_no = 1,
      edge_color_var = "weight",
      edge_width_var = "weight",
      edge_zoom = 2
    )
    
  }, NA)
})

test_that("plot_ego_gram works without pie_var/venn_var", {
  # This is meant more as a manual test. Plots should appear immediately.
  expect_error({
    e <- make_egor(5, 32)
    
    plot_egograms(
      e,
      ego_no = 1,
      venn_var = "sex",
      pie_var = NULL,
      vertex_color_var = "sex",
      edge_color_var = "weight",
      edge_width_var = "weight",
      edge_zoom = 3
    )
    
    plot_egograms(
      e,
      ego_no = 1,
      venn_var = NULL,
      pie_var = "sex",
      vertex_color_var = "sex",
      edge_color_var = "weight",
      edge_width_var = "weight",
      edge_zoom = 3
    )
    
  }, NA)
  
  expect_warning({
    plot_egograms(
      e,
      ego_no = 1,
      venn_var = NULL,
      pie_var = NULL,
      vertex_color_var = "sex",
      edge_color_var = "weight",
      edge_width_var = "weight",
      edge_zoom = 3
    )
  })
})

test_that("plot_ego_gram plots empty levels of a factor variables for pies and venns",
          {
            # This is meant more as a manual test. Plots should appear immediately.
            expect_error({
              e <- make_egor(50, 12)
              e <- e %>%
                activate(alter) %>%
                mutate(age2 = as.character(age),
                       rating = sample(1:5, n(), replace = TRUE))
              e <-
                e %>%
                mutate(rating.f = factor(rating, levels = 1:5))
              plot_egograms(
                e,
                ego_no = 4,
                venn_var = "rating",
                pie_var = "age2",
                vertex_color_var = "sex",
                edge_color_var = "weight",
                edge_width_var = "weight",
                edge_zoom = 3,
                venn_gradient_reverse = FALSE
              )
              
              plot_egograms(
                e,
                ego_no = 1,
                venn_var = "rating",
                pie_var = "sex",
                vertex_color_var = "sex",
                edge_color_var = "weight",
                edge_width_var = "weight",
                edge_zoom = 3,
                #highlight_box_col_var = "country"
              )
              
              plot_egograms(
                e,
                ego_no = 1,
                venn_var = "rating",
                pie_var = "sex",
                vertex_color_var = "sex",
                edge_color_var = "weight",
                edge_width_var = "weight",
                edge_zoom = 3,
                vertex_label_var = NULL
                #highlight_box_col_var = "country"
              )
              
            }, NA)
            layout_egogram(e$alter$.altID, e$alter$age, e$alter$rating.f)
            expect_warning({
              plot_egograms(
                e,
                ego_no = 1,
                venn_var = NULL,
                pie_var = NULL,
                vertex_color_var = "sex",
                edge_color_var = "weight",
                edge_width_var = "weight",
                edge_zoom = 3
              )
            })
          })

test_that("ego-alter weights are plotted",
          {
            e <- make_egor(5, 12)
            e$alter$weight <-
              sample(1:5 / 5, nrow(e$alter), replace = TRUE)
            expect_error({
              plot_ego_graphs(e, include_ego = TRUE)
              plot_ego_graphs(x = e, edge_width_var = "weight", include_ego = TRUE)
            }, NA)
            expect_error({
              plot_ego_graphs(e, include_ego = TRUE)
              plot_ego_graphs(e, edge_color_var = "weight", include_ego = TRUE)
            }, NA)
          })

test_that("egograms with many venns produce adequatly sized nodes",
          {
            data("transnat")
            transnat <-
              transnat %>%
              activate(alter) %>%
              mutate(test_var = sample(1:12, nrow(.$alter), replace = TRUE))
            expect_error(
              plot_egograms(
                transnat,
                venn_var = "test_var",
                pie_var = "sex",
                vertex_zoom = 1,
                vertex_label_var = NULL
              ),
              NA
            )
          })

test_that("egograms with `include_ego = TRUE` work properly", {
  expect_error(plot_egograms(x = egor32,
                             venn_var = "age",
                             pie_var = "country",
                             include_ego = TRUE), NA)
  
  expect_error(plot_egograms(x = egor32,
                             venn_var = "age",
                             pie_var = "country",
                             include_ego = FALSE), NA)
})

test_that("egograms with reverse ordered alters plot correctly", {
  # This (correct) version of the plot...
  plot_egograms(egor32, venn_var = "age", pie_var = "country")
  
  # ...should look exactly like this version.
  egor32 %>%
    activate(alter) %>%
    arrange(.egoID, desc(.altID)) %>%
    plot_egograms(venn_var = "age", pie_var = "country")
  
  ego <-
    tibble(egoID = c("Hans", "Peter", "Klaus"),
           var = c(1, 2, 3))
  
  alter <-
    tibble(
      egoID = c(rep("Hans", 3), rep("Peter", 3), rep("Klaus", 3)),
      alterID = c(
        "Mary",
        "Paul",
        "Susana",
        "Irna",
        "Laser3000",
        "Pferd",
        "Ross",
        "Ricky",
        "Herald"
      ),
      var1 = sample(1:3, 9, replace = TRUE),
      var2 = sample(1:3, 9, replace = TRUE)
    )
  
  e1 <- egor(alter, ego)
  
  plot_egograms(e1,
                venn_var = "var1",
                pie_var = "var2",
                ego_no = 2)
  

  expect_error(  e1 %>%
                   activate(alter) %>%
                   arrange(.egoID, desc(.altID)) %>%
                   plot_egograms(venn_var = "var1",
                                 pie_var = "var2",
                                 ego_no = 2), NA)
})

dev.off() # Closing the NULL pdf device.