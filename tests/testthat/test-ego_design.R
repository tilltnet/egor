test_that("left_join works with srvyr ego object", {
  x <- make_egor(5, 32)
  
  x$ego$sampling_weight <- sample(1:10/10, 5, replace = TRUE)
  ego_design(x) <- list(weight = "sampling_weight")
  
  res <- composition(x, age)
  
  left_join(x, res, copy = TRUE)
})
