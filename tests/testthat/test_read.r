library(egonetR)

# One File
path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egonetR")
egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)

attr.start.col <- which(names(egos_8) == "alter.sex.1")
attr.end.col <- which(names(egos_8) == "alter.age.8")
dy.first.var <- which(names(egos_8) == "X1.to.2")

of <- read.egonet.one.file(egos_8, egos_8$netsize, 
                           attr.start.col = attr.start.col, 
                           attr.end.col = attr.end.col, 
                           dy.first.var = dy.first.var,
                           dy.max.alteri = 8)

# Two Files
path_to_alteri_8.csv <- system.file("extdata", "alteri_8.csv", package = "egonetR")
alteri_8 <- read.csv2(path_to_alteri_8.csv, row.names = 1)

tf <- read.egonet.two.files(egos = egos_8, alteri = alteri_8, e.max.alteri = 8,
                            e.first.var = dy.first.var)

# Folders
path_to_edges_folder <- system.file("extdata", "edges_32", package = "egonetR")
path_to_alteri_folder <- system.file("extdata", "alteri_32", package = "egonetR")
path_to_egos_32.csv <- system.file("extdata", "egos_32.csv", package = "egonetR")

fo <- read.egonet.folders(egos.file = path_to_egos_32.csv, 
                          edge.folder = paste(path_to_edges_folder, "/", sep = ""),
                          alter.folder = paste(path_to_alteri_folder, "/", sep = ""),
                          first.col.row.names = TRUE, csv.sep = ";")



