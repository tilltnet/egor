cat(" \nTestfile  test_openeddi.r \n")

library(egor)
# Example
setwd(system.file("extdata", "openeddi", package = "egor"))
oe <- read.openeddi()
