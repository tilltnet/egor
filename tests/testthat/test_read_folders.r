egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
alteri.folder <- system.file("extdata", "alteri_32", package = "egor")
edges.folder <-  system.file("extdata", "edges_32", package = "egor")

ef <- read.egonet.folders(egos.file = egos.file, alter.folder = alteri.folder, edge.folder = edges.folder, csv.sep = ";")

