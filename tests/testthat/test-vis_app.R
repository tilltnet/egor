if (FALSE) {
  library(purrr)
  library(egor)
  f <- make_egor(50, 50)
  ab <- filter(f, sex == "w")
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
  
  egor_vis_app()
}
