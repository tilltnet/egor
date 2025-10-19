test_that("as_alters_df works",
          {
            expect_error({
              
              as_alters_df(egor32, include.ego.vars = TRUE)
              as_aaties_df(egor32)
              
            }, NA)
          })

test_that("as_tibble and other conversions work",
          {
            expect_error({
              
              as_tibble(egor32)
              activate(egor32, "aatie") %>%
                as_tibble(include.ego.vars = TRUE)
              
              activate(egor32, "alter") %>%
                as_tibble(include.ego.vars = TRUE)
              
              activate(egor32, "aatie") %>%
                as_tibble(include.alter.vars = TRUE)
              x <- activate(egor32, "aatie")
              
            }, NA)
          })

test_that("as_alters_df works.",
          {
            expect_error(as_alters_df(egor32), NA)
            expect_error(as_alters_df(egor32, include.ego.vars = T), NA)
          })

test_that("as_aaties_df works.",
          {
            expect_error(as_aaties_df(egor32), NA)
            expect_error(as_aaties_df(object = egor32, include.alter.vars = T), NA)
            expect_error(as_aaties_df(object = egor32, include.ego.vars = T), NA)
            expect_error(as_aaties_df(
              object = egor32,
              include.ego.vars = T,
              include.alter.vars = TRUE
            ),
            NA)
          })
