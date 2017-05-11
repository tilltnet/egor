egos.file <-  system.file("extdata", "egos_32.csv", package = "egor")
alteri.file <- system.file("extdata", "alteri_32.csv", package = "egor")
edges.file <-  system.file("extdata", "edges_32.csv", package = "egor")

egos <- read.csv2(egos.file)
alteri <- read.csv2(alteri.file)
edges <- read.csv2(edges.file)

read.egonet.three.files(egos = egos, alteri.df = alteri, edges = edges)

