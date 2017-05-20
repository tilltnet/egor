e1_gen <- generate.sample.ego.data(32, 20)

test_that("e1_gen is egor object", 
          expect_true((class(e1_gen) == "egor")[1]))


# Draw/ Extract egos and gobal alters/alter-tie dfs from egor
egos <- select(e1_gen, -.alters, -.alter_ties)
alters <- tidyr::unnest(e1_gen[,c(1,5)])
alter_ties <- tidyr::unnest(e1_gen[,c(1,6)])

e1 <- egor(alters, egos, alter_ties)
class(e1) # double egor :(
summary(e1_gen)
summary(e1)

test_that("density values macht up", 
          all(ego_density(e1_gen) == ego_density(e1)))

test_that("weighted density values macht up", 
          expect_true(
            all(ego_density(e1_gen, weight = "weight") == 
                  ego_density(e1, weight = "weight"))))

ego_density(e1$.alter_ties, e1$.alters, weight = "weight")
ego_density(alter_ties, alters, weight = "weight")



err_d <- generate.sample.ego.data(32, 20)


# Checking for egos without alters
alters <- unnest(select(err_d, egoID, .alters))
alter_ties <- unnest(select(err_d, egoID, .alter_ties))
alters <- alters[!alters$egoID %in% c(1,2,3), ]
egos <- dplyr::select(err_d, -.alters, -.alter_ties)
err_d1 <- egor(alters, egos, alter_ties)



# Checking egos without alter_ties
alter_ties <- alter_ties[!alter_ties$egoID %in% c(4,5,9), ]
err_d2 <- egor(alters, egos, alter_ties)


# Checking for alters and alter_ties without egos
egos <- select(err_d, -.alters, -.alter_ties)
egos <- egos[sample(egos$egoID, 24), ]
err_d3 <- egor(alters, egos, alter_ties)

# Non-Unique egoID in ego data
egos <- rbind(egos, cbind(egos[1:4, 1], egos[5:8, -1] ))
err_d4 <- egor(alters, egos, alter_ties)

summary(err_d4)
ego_density(err_d4)

