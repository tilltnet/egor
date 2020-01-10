## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(egor)
library(purrr)
library(haven)

## -----------------------------------------------------------------------------
data("allbus_2010_simulated")
raw_data <- allbus_2010_simulated

## -----------------------------------------------------------------------------
var_labels <- map_chr(raw_data, ~attr(., "label"))

var_labels <- gsub("[,\\.:;><?+()-]", " ", var_labels)
var_labels <- gsub("\\s+", "_", trimws(var_labels))

## -----------------------------------------------------------------------------
var_labels <- gsub("FREUND_IN_._", "", var_labels)
var_labels <- gsub("^KONTAKT_._", "", var_labels)

## -----------------------------------------------------------------------------
names(raw_data) <- make.unique(var_labels, sep = "")

## -----------------------------------------------------------------------------
split_freunde <- 
  raw_data %>% 
  filter(FRAGEBOGENSPLIT_F020 == 1)

## -----------------------------------------------------------------------------
e_freunde <- onefile_to_egor(
                egos = split_freunde,
                ID.vars = list(ego = "IDENTIFIKATIONSNUMMER_DES_BEFRAGTEN"),
                netsize = split_freunde$ANZ_GENANNTER_NETZWERKPERS_SPLIT_1,
                attr.start.col = "GESCHLECHT",
                attr.end.col = "SPANNUNGEN_KONFLIKTE2",
                aa.first.var = "KENNEN_SICH_A_B",
                max.alters = 3)

## -----------------------------------------------------------------------------
attr(raw_data$KENNEN_SICH_A_B, "labels")

## -----------------------------------------------------------------------------
e_freunde <- 
  e_freunde %>% 
  activate(aatie) %>% 
  filter(weight != 2) %>% 
  activate(ego)

## -----------------------------------------------------------------------------
split_kontakte <- 
  raw_data %>% 
  filter(FRAGEBOGENSPLIT_F020 == 2) 

e_kontakte <- onefile_to_egor(
  egos = split_kontakte,
  ID.vars = list(ego = "IDENTIFIKATIONSNUMMER_DES_BEFRAGTEN"),
  netsize = split_kontakte$ANZ_GENANNTER_NETZWERKPERS_SPLIT_2,
  attr.start.col = "GESCHLECHT3",
  attr.end.col = "SPANNUNGEN_KONFLIKTE7",
  aa.first.var = "KENNEN_SICH_KONTAKT_A_B",
  max.alters = 5)

e_kontakte <- 
  e_kontakte %>% 
  activate(aatie) %>% 
  filter(weight != 3) %>% 
  activate(ego)

## -----------------------------------------------------------------------------
plot(e_freunde, ego_no = 4, x_dim = 2, y_dim = 1)
plot(e_kontakte, ego_no = 4, x_dim = 2, y_dim = 1)

## -----------------------------------------------------------------------------
e_freunde <- 
  e_freunde%>% 
  activate(alter) %>% 
  mutate(WO_GEBOREN = droplevels(as_factor(WO_GEBOREN)),
         KONTAKTE =  droplevels(as_factor(KONTAKTE)))

plot_egograms(e_freunde, 
              ego_no = 4, 
              x_dim = 1, 
              y_dim = 1, venn_var = "KONTAKTE",
              pie_var = "WO_GEBOREN")

e_kontakte <- 
  e_kontakte %>% 
  activate(alter) %>% 
  mutate(WO_GEBOREN = droplevels(as_factor(WO_GEBOREN)),
         KONTAKTE =  droplevels(as_factor(KONTAKTE)))

plot_egograms(e_kontakte, 
              ego_no = 4, 
              x_dim = 1, 
              y_dim = 1, 
              venn_var = "KONTAKTE" ,
              pie_var = "WO_GEBOREN")

