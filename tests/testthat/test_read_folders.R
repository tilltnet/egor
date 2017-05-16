egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
alters.folder <- system.file("extdata", "alters_32", package = "egor")
edges.folder <-  system.file("extdata", "edges_32", package = "egor")
<<<<<<< HEAD:tests/testthat/test_read_folders.r
library(egor)
ef <- read.egonet.folders(egos.file = egos.file, alter.folder = alteri.folder, edge.folder = edges.folder, csv.sep = ";")
=======

ef <- read.egonet.folders(egos.file = egos.file, alter.folder = alters.folder, edge.folder = edges.folder, csv.sep = ";")
>>>>>>> 92f476aaa4b6195de1556e5486f2e8da4e13ec37:tests/testthat/test_read_folders.R

alter_ties <- mapply(FUN = function(x, y) data.frame(egoID = y, x), ef$edges, ef$egos.df$egoID, SIMPLIFY = F)

alter_ties.df <- do.call(rbind, alter_ties)



egor(alters.df = ef$alteri.df, egos.df = ef$egos.df, alter_ties.df)
egor(alters.df = ef$alteri.df, egos.df = ef$egos.df)
egor(alters.df = ef$alteri.df)
