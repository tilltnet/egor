asd <- generate.sample.ego.data(32, 20)
alters.df <- asd$alters
egos.df <-  asd[ , -c(2,3)]
alters.df$egoID <- as.integer(alters.df$egoID)
alter_ties <- mapply(FUN = function(x, y) data.frame(egoID = y, x), asd$alter_ties, 1:length(asd$alter_ties), SIMPLIFY = F)

alter_ties.df <- do.call(rbind, alter_ties)




library(tidyr)
library(dplyr)


class(alters.df$egoID)
class(egos.df$egoID)
class(alter_ties.df$egoID)


e1 <- egor(alters.df, egos.df, alter_ties.df)
summary(e1)
ego_density(e1)
ego_density(e1, weight = "weight")

ego_density(e1$alter_ties, e1$alters, weight = "weight")
ego_density(alter_ties.df, alters.df, weight = "weight")


NROW(e1$alter_ties[[1]])
NROW(e1$alters[[1]])
x <- e1$alter_ties
weight = "weight"



err_d <- generate.sample.ego.data(32, 20)


# Checking for egos without alters
alters <- unnest(err_d[1:2])
alter_ties <- unnest(err_d[c(1,3)])
alters <- alters[!alters$egoID %in% c(1,2,3), ]

err_d1 <- egor(alters, err_d[c(-2, -3)], alter_ties)
View(err_d1)

View(unnest(err_d1[1:2]))


# Checking egos without alter_ties
alter_ties <- alter_ties[!alter_ties$egoID %in% c(4,5,9), ]
err_d2 <- egor(alters, err_d[c(-2, -3)], alter_ties)


# Checking for alters and alter_ties without egos
egos <- err_d[c(-2, -3)]
egos <- egos[sample(egos$egoID, 24), ]
err_d3 <- egor(alters, egos, alter_ties)

# Non-Unique egoID in ego data
egos <- err_d[c(-2, -3)]
egos <- rbind(egos, cbind(egos[1:4, 1], egos[5:8, -1] ))
err_d4 <- egor(alters, egos, alter_ties)

summary(err_d4)
ego_density(err_d4)


# filter
cond_1 <- "alter.sex"
cond_2 <- "w"
e1$alters[[1]][cond ==, ]
lapply(X= e1$alters, FUN = function(x, att, cond) 
  x[x[att] == cond, ]
, att = "alter.sex", cond = "w")


dplyr::filter(e1$alters[[1]], alter.sex == "w")


filter()

if

dplyr::filter_

class(~13<2)
lm


cond <- ~alter.sex == "w"
if(eval(cond, parent.frame())) {}
cond
model.matrix.default
