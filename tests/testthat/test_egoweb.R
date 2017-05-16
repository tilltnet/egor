setwd("C:/Users/Till/ownCloud/Diss/rwd/read_egonet/read egoweb")
setwd("C:/Users/K.Seiz/ownCloud/Diss/rwd/read_egonet/read egoweb")
library(egor)
# This Line produces an error in the creation of the igraph objects, because of
# duplicate alter names/ vertex names.
egoW <- read.egoweb(alter.file = "example egoweb ego alter data with stats exported.csv", edges.file = "example egoweb edgelist.csv")

# Check if alters entries with duplicated alterID ("Alter.Number") exist
dup_list <- lapply(egoW$alters.list, FUN = function(x) !all(!duplicated(x$Alter.Number)))
dup_names <- names(egoW$alters.list)[as.logical(dup_list)]
index <- which(names(egoW$alters.list) == dup_names)
egoW$alters.list[[index]]
# -> "SWF_F08_Baseline" is a duplicated egoID ('EgoID')

# Using 'Interview.ID' instead of 'EgoID' seems to fix the problem.
egoW2 <- read.egoweb(alter.file = "example egoweb ego alter data with stats exported.csv", 
                     edges.file = "example egoweb edgelist.csv", 
                     ID.vars = c("Interview.ID", "Alter.Number", "Alter.1.Number", "Alter.2.Number"))
# But the entries for one ego and its alters are still duplicated.  
egoW2$egos$EgoID[duplicated(egoW2$egos$EgoID)]

# Let's open the alter data, exclude the duplicated interview and write back to 
# to disk as a new CSV file.
alters_df <- read.csv("example egoweb ego alter data with stats exported.csv")
alters_df <- alters_df[alters_df$Interview.ID != 1653, ]
write.csv(alters_df, file = "alter_data.csv", row.names = F)
# Finally:
egoW3 <- read.egoweb(alter.file = "alter_data.csv", 
                     edges.file = "example egoweb edgelist.csv")


# Example
setwd(system.file("extdata", "egoweb", package = "egor"))
ew <- read.egoweb(alter.file = "alters_32.csv", edges.file = "edges_32.csv", egos.file = "egos_32.csv")
