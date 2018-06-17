context("test_egor.R")

library(egor)
library(testthat)
e1_gen <- make_egor(32, 20)

test_that("e1_gen is egor object", 
          expect_true((class(e1_gen) == "egor")[1]))


# Draw/ Extract egos and gobal alters/alter-tie dfs from egor
egos <- dplyr::select(e1_gen, -.alts, -.aaties)
alters <- tidyr::unnest(tibble::as_tibble(e1_gen)[, c("egoID",".alts")])
aaties <- tidyr::unnest(tibble::as_tibble(e1_gen)[, c("egoID",".aaties")])

names(alters)[2] <- "alterID"
names(aaties)[2:3] <- c("Source", "Target")

e1 <- egor(alters, egos, aaties)
summary(e1_gen)
summary(e1)

test_that("density values macht up", 
          expect_true(all(ego_density(e1_gen) == ego_density(e1))))

test_that("weighted density values macht up", 
          expect_true(
            all(ego_density(e1_gen, weight = "weight") == 
                  ego_density(e1, weight = "weight"))))

ego_density(e1$.aaties, e1$.alts, weight = "weight")
ego_density(aaties, alters, weight = "weight")



err_d <- make_egor(32, 20)


# Checking for egos without alters
alters <- tidyr::unnest(tibble::as_tibble(e1_gen)[, c("egoID",".alts")])
aaties <- tidyr::unnest(tibble::as_tibble(e1_gen)[, c("egoID",".aaties")])
names(alters)[2] <- "alterID"
names(aaties)[2:3] <- c("Source", "Target")
alters <- alters[!alters$egoID %in% c(1,2,3), ]
egos <- dplyr::select(err_d, -.alts, -.aaties)
err_d1 <- egor(alters, egos, aaties)



# Checking egos without aaties
aaties <- aaties[!aaties$egoID %in% c(4,5,9), ]
err_d2 <- egor(alters, egos, aaties)


# Checking for alters and aaties without egos
egos <- dplyr::select(err_d, -.alts, -.aaties)
egos <- egos[sample(egos$egoID, 24), ]
err_d3 <- egor(alters, egos, aaties)

# Non-Unique egoID in ego data (should raise warning)
egos <- rbind(egos, cbind(egos[1:4, 1], egos[5:8, -1] ))

expect_warning(err_d4 <- egor(alters, egos, aaties))

summary(err_d4)
ego_density(err_d4)

