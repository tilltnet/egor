egonetR
=======

R Package for importing and analysing ego-centric-network data.

This is still under development.


Install
-------
    library(devtools)
    install_github(repo="tilltnet/egonetR")

Importing ego-centric network data
----------------------------
There are currently three commands for importing ego-centric network data:
    
    read.egonet.one.file(egos, netsize,  netID = "netID", attr.start.col, attr.end.col, dy.max.alteri, dy.first.var)
    read.egonet.two.files(egos, long, netsize = NULL,  netID = "netID", alterID = NULL, dy.max.alteri, dy.first.var)
    read.egonet.folders(egos, alter.folder, edge.folder, netID = "netID")
    
Each command produces a *list* of the following objects:

  - ego-*dataframe*, containing all egos, their attributes and other items
  - *long* tie-*dataframe*, containg all alteri and their attributes
  - *list*, containing a *dataframe* for each ego/ network (similar to the tie-*dataframe* before, but different structure)
  - edge*list*, containg all edges between the alteri (+ edge attributes)
  - *list* of igraph objects representing the networks
  - result-*dataframe*, pre-populated with netsize

Example (without importing data)
-------

    # Generate 500 random networks, each with 5 alteri.
    et <- generate.sample.ego.data(net.count = 500, max.alteri = 5)
    names(et)
    
    # Extract elements of et for convenience.
    net <- et$net
    long <- et$long
    edges <- et$edges
    
    # Calculate Netsize (size = max.alteri so far)
    netsize <- aggregate(long$netID, by = list(long$netID), FUN = function(x) NROW(x))[[2]]
    
    # Calculate compositional information per network for the variable age.
    age_comp <- composition(long.df = long, v_alt = "alter.age", netsize = netsize, v_ego = net$age,  egoID = "netID")
    net <- data.frame(net, age_comp , check.names = F)
    
    barplot(prop.table(table(net$diversity)), main = "Age Diversity")
    barplot(prop.table(table(net$EI)), main = "EI-Index")