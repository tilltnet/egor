context("test_read.R")

test_that(
  "onefile_to_egor() works.",
  {
    path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
    egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
    
    attr.start.col <- which(names(egos_8) == "alter.sex.1")
    attr.end.col <- which(names(egos_8) == "alter.age.8")
    dy.first.var <- which(names(egos_8) == "X1.to.2")
    
    expect_error(onefile_to_egor(
      egos = egos_8, netsize = egos_8$netsize, 
      attr.start.col = attr.start.col, 
      attr.end.col = attr.end.col, 
      aa.first.var = dy.first.var,
      max.alters = 8)
      ,NA)
  }
)

test_that(
  "twofiles_to_egor() works.",
  {
    path_to_alters_8.csv <- system.file("extdata", "alters_8.csv", package = "egor")
    path_to_one_file_8 <- system.file("extdata", "one_file_8.csv", package = "egor")
    egos_8 <- read.csv2(path_to_one_file_8, row.names = 1)
    alters_8 <- read.csv2(path_to_alters_8.csv, row.names = 1)
    
    #attr.start.col <- which(names(egos_8) == "alter.sex.1")
    #attr.end.col <- which(names(egos_8) == "alter.age.8")
    dy.first.var <- which(names(egos_8) == "X1.to.2")
    
    expect_error(
      twofiles_to_egor(
        egos = egos_8,
        alters = alters_8,
        e.max.alters = 8,
        e.first.var = dy.first.var),
      NA)
    
    alters_8$selection1 <-
      sample(c(TRUE, FALSE), nrow(alters_8), replace = TRUE)
    alters_8$selection2 <-
      sample(c(1, 0), nrow(alters_8), replace = TRUE)
    alters_8$selection3 <-
      sample(c(1, NA), nrow(alters_8), replace = TRUE)

    expect_error(
      a <- twofiles_to_egor(
        egos = egos_8,
        alters = alters_8,
        ID.vars = list(
          ego = "egoID",
          alter = "alterID_",
          source = "Source",
          target = "Target"
        ),
        e.max.alters = 8,
        e.first.var = dy.first.var,
        selection = "selection1"),
        
      NA)
    
    expect_error(
      a <- twofiles_to_egor(
        egos = egos_8,
        alters = alters_8,
        ID.vars = list(
          ego = "egoID",
          alter = "alterID_",
          source = "Source",
          target = "Target"
        ),
        e.max.alters = 8,
        e.first.var = dy.first.var,
        selection = "selection2"),
      
      NA)
    
    expect_error(
      a <- twofiles_to_egor(
        egos = egos_8,
        alters = alters_8,
        ID.vars = list(
          ego = "egoID",
          alter = "alterID_",
          source = "Source",
          target = "Target"
        ),
        e.max.alters = 8,
        e.first.var = dy.first.var,
        selection = "selection3"),
      
      NA)
  }
)

# test_that("twofiles_to_egor works with transnat data", {
#   skip_if_not(Sys.info()["nodename"] == "Tills-MacBook-Pro.local")
#   #alteri_raw <- foreign::read.spss("/Users/tillkrenz/Dropbox/Nextcloud/clientsync/Lehre/egor Workshop/egor_sunbelt_18/02_tie.sav", to.data.frame = TRUE, use.value.labels = FALSE)
#   #egos_raw <- foreign::read.spss("/Users/tillkrenz/Dropbox/Nextcloud/clientsync/Lehre/egor Workshop/egor_sunbelt_18/02_net.sav", to.data.frame = T, use.value.labels = F)
#   #save(alteri_raw, egos_raw, file = "/Users/tillkrenz/Dropbox/egor_kram/transnat_raw_data.rda")
#   load("~/Dropbox/egor_kram/transnat_raw_data.rda")
#   alter.alter <- egos_raw[12:39]
#   sorted_alter_alter <- sort(names(alter.alter))
#   egos_raw <- data.frame(egos_raw[1:11], egos_raw[sorted_alter_alter])
#   
#   which(names(egos_raw) == "b10_1_2") # Insert variable name of first alter-alter variable.
#   transnat <- twofiles_to_egor(egos = egos_raw, 
#                                alters = alteri_raw, 
#                                e.max.alters = 8,
#                                e.first.var = 12, 
#                                selection = "selected")
#   a <- transnat$aatie %>% 
#     filter(.egoID == 12) %>% 
#     {c(.$.srcID, .$.tgtID)} %>% 
#     sort() %>% 
#     unique() %>% 
#     as.numeric()
#   b <- alteri_raw %>% filter(egoID == 12, selected == 1) %>% pull(alterID)
#   expect_true(all(a %in% b))
# })

test_that(
  "read_egonet() works.",
  {
    path_to_edges_folder <- system.file("extdata", "edges_32", package = "egor")
    path_to_alters_folder <- system.file("extdata", "alters_32", package = "egor")
    path_to_egos_32.csv <- system.file("extdata", "egos_32.csv", package = "egor")
    
    expect_error(
      read_egonet(
        egos.file = path_to_egos_32.csv, 
        edge.folder = paste(path_to_edges_folder, "/", sep = ""),
        alter.folder = paste(path_to_alters_folder, "/", sep = ""),
        first.col.row.names = FALSE, csv.sep = ";")
      , NA)
  }
)


# Test Cases to build
# - Cases without network information
# - egoID as factor/numeric/character
# - one file: var.wise alter attributes
