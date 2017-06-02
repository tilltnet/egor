library(egor)

eigor <- generate.sample.ego.data(32, 20)


EI(eigor, var_name = "alter.sex")

aaties <- eigor$.aaties
alters <- eigor$.alters

EI(alters, aaties, var_name = "alter.sex")

