egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
alteri.folder <- system.file("extdata", "alteri_32", package = "egor")
edges.folder <-  system.file("extdata", "edges_32", package = "egor")
library(egor)
ef <- read.egonet.folders(egos.file = egos.file, alter.folder = alteri.folder, edge.folder = edges.folder, csv.sep = ";")

alter_ties <- mapply(FUN = function(x, y) data.frame(egoID = y, x), ef$edges, ef$egos.df$egoID, SIMPLIFY = F)

alter_ties.df <- do.call(rbind, alter_ties)



egor(alters.df = ef$alteri.df, egos.df = ef$egos.df, alter_ties.df)
egor(alters.df = ef$alteri.df, egos.df = ef$egos.df)
egor(alters.df = ef$alteri.df)
