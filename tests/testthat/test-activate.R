test_that("activate works with singulars and plurals of level names", {

  expect_error(  egor32 %>% 
                   activate(ego), NA)
  
  # expect_error(  egor32 %>% 
  #                  activate(egos), NA)
})
