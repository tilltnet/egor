egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
alters.folder <- system.file("extdata", "alters_32", package = "egor")
edges.folder <-  system.file("extdata", "edges_32", package = "egor")
library(egor)
ef <- read.egonet.folders(egos.file = egos.file, alter.folder = alters.folder, edge.folder = edges.folder, csv.sep = ";")




