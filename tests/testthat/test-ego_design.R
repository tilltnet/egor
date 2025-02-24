test_that("left_join works with srvyr ego object", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  res <- composition(x, age)
  
  expect_error(left_join(x, res, copy = TRUE), NA)
})


test_that("summary.egor works with ego_design", {
  x <- make_egor(5, 32)
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  x |> 
    activate(alter) |> 
    summarise(.n = n(), .by = .egoID)
  
  y <- 
    x |> 
    activate(alter) |> 
    summarise(.n = n(), .by = .egoID)  
  
  xxx <-
    bind_cols(
      .egoID = x$ego$variables$.egoID,
      .cluster = x$ego$cluster[[1]],
      .strata = x$ego$strata[[1]],
      .allprob = x$ego$allprob[[1]]
    ) 
  
  
  survey::left_join(x$ego,
            x |> 
              activate(alter) |> 
              summarise(.n = n(), .by = .egoID))
  
  as_alters_survey(x) |>
    srvyr::summarise(srvyr::n(), .by = .egoID)
  
  as_alters_survey(x) |>
    srvyr::summarise(n(), .by = .egoID)
  
  
  
  x |> 
    activate(alter) |> 
    summarise(.n = n(), .by = .egoID) |> 
    # left_join(xxx) |> 
    # srvyr::as_survey(strate = .strata,
    #                  cluster = .cluster,
    #                  probs = .allprob) |> 
    summarise(avg_netsize = mean(.n))
  
  x |> 
    activate(alter) |> 
    summarise(.n = n(), .by = .egoID) |> 
    left_join(xxx) |> 
    srvyr::as_survey(strate = .strata,
                     cluster = .cluster,
                     probs = .allprob) |> 
    srvyr::summarise(avg_netsize = srvyr::survey_mean(.n))
  
  expect_error(summary(object = x), NA)
  expect_warning(summary(object = x), NA)
})

test_that("ego_density works with ego_design", {
  x <- make_egor(5, 32)

  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  library(srvyr)
  expect_error(ego_density(x), NA)

  ## tidyselect arguments work as well
  ego_design(x) <- alist(weight = sampling_weight)
  expect_error(ego_density(x), NA)
})

test_that("survey_mean and svymean work with ego_design", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  options(egor.results_with_design = TRUE)

  expect_error(ego_density(x) %>% 
    srvyr::summarise(mean_dens = srvyr::survey_mean(density)), NA)
  
  expect_error(survey::svymean(~density, ego_density(x)), NA)
})

o <- options(useFancyQuotes = FALSE, width = 999)

test_that("sensible error message is produced when using list() instead of alist()", {
  expect_error(
    egor(alters32, egos32, aaties32,
         ID.vars = list(ego = ".EGOID", alter = ".ALTID", source = ".SRCID", target =  ".TGTID"),
         ego_design = list(strata = sex)),
    ".* Did you pass ego design variable names unquoted and wrap them in 'list\\(\\)' rather than 'alist\\(\\)'\\?.*"
  )
})

options(o)
