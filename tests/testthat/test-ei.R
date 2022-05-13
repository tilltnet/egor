test_that("EI() works.",
          {
          skip_on_cran()
            eigor <- make_egor(net.count = 6, max.alters = 20)
            
            expect_error(EI(object = eigor, alt.attr = "age"), NA, label = "regular")
            expect_error(EI(eigor, alt.attr = "sex"), NA, label = "regular 2")
            expect_error(EI(eigor, alt.attr = sex), NA, label = "NSE input.")
            
            eigor$alter$int_var <-
              sample(1:3, NROW(eigor$alter), replace  = T)
            
            expect_error(EI(eigor, alt.attr = "int_var"), NA, "non-factor variable")
            
            # Issue #16 by mbojan
            egodf <- data.frame(id_ego = 1:2,
                                female = 0:1)
            alterdf <- data.frame(
              id_ego = c(1, 1),
              id_alter = 1:2,
              female = c(FALSE, TRUE)
            )
            aaties <- data.frame(
              id_ego = 1,
              from = 1,
              to = 2,
              close = 100
            )
            e <- egor(
              alterdf,
              egodf,
              aaties,
              ID.vars = list(
                ego = "id_ego",
                alter = "id_alter",
                source = "from",
                target = "to"
              )
            )
            expect_error(EI(object = e, alt.attr = "female"),
                         NA, label = "Missing aaties.")
            
            # same test with more complete aaties data but only one group in the network
            alterdf <- data.frame(
              id_ego = rep(1, 3),
              id_alter = 1:3,
              female = rep(TRUE, 3)
            )
            aaties <- data.frame(
              id_ego = rep(1, 3),
              from = c(1, 1, 2),
              to = c(2, 3, 3),
              close = rep(100, 3)
            )
            e <- egor(
              alterdf,
              egodf,
              aaties,
              ID.vars = list(
                ego = "id_ego",
                alter = "id_alter",
                source = "from",
                target = "to"
              )
            )
            expect_error(EI(object = e, alt.attr = "female"),
                         NA, label = "Missing aaties.")
          })

test_that("EI() works with include.ego",
          {
            skip_on_cran()
            eigor <- make_egor(net.count = 6, max.alters = 20)
            
            expect_error({
              EI(object = eigor, alt.attr = "age")
              EI(object = eigor,
                 alt.attr = "age",
                 include.ego = TRUE)
            }, NA)
          })

test_that("EI() works rescale TRUE/FALSE",
          {
            skip_on_cran()
            eigor <- make_egor(net.count = 6, max.alters = 20)
            expect_error({
            EI(object = eigor, alt.attr = "age", rescale = FALSE)
            EI(object = eigor, alt.attr = "age")
            }, NA)
          })

test_that("comp_ei handles extreme values (only one group in alts) correctly",
          {
            skip_on_cran()
            egor32 <- make_egor(32, 10)
            
            make_nas <- function(var) {
              if (is_tibble(var)) var <- var[[1]]
              var[sample(1:length(var), 1)] <- NA
              var
            }
            
            #egor32$ego$sex <- make_nas(egor32$ego$sex)
            egor32$alter$sex <- make_nas(egor32$alter$sex)
            
            egor32 <- 
              egor32 %>% 
              activate("alter") %>% 
              mutate(sex = case_when(
                .egoID == 1 ~ "w",
                .egoID == 2 ~ "m",
                .egoID == 3 ~ "w",
                TRUE ~ as.character(sex)
              ))
            
            
            expect_true(all(abs(comp_ei(object = egor32,
                                        alt.attr = "sex", 
                                        ego.attr = "sex")$ei[1:3]) == 1))
          })

test_that("comp_ei handles character vectors correctly",
          {
            skip_on_cran()
            e1 <- make_egor(32,32)
            egor32 <- e1
            e1$ego$sex <- as.character(e1$ego$sex)
            e1 <- e1 %>% 
              activate("alter") %>%
              mutate(sex = as.character(sex))
            
            #e1 %>% as_tibble() %>% View()
            
            expect_equal(comp_ei(e1, "sex", ego.attr = "sex"),
                  comp_ei(egor32, "sex",ego.attr = "sex"))
            
          })

test_that("EI() ungroups data first", {
  skip_on_cran()
  eigor <- make_egor(net.count = 6, max.alters = 20)
  expect_error(eigor %>% 
    activate(aatie) %>% 
    group_by(.egoID) %>% 
    EI(alt.attr = "age"), NA)
  })

test_that("EI() returns tbl_svy object, when ego_design present", {
  skip_on_cran()
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <-
    sample(1:10 / 10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  options(egor.results_with_design = TRUE)
  res <- EI(object = x, alt.attr =  "sex")
  expect_is(res, "tbl_svy")
})


test_that("EI() ungroups data first", {
  skip_on_cran()
  data("egor32")
  expect_error(egor32 %>% 
                 activate(aatie) %>% 
                 group_by(.egoID) %>% 
                 EI(alt.attr = "age"), NA)
})