egonetR
=======

R Package for importing and analysing ego-centric-network data.

This is still under development.


Install
-------
    library(devtools)
    install_github(repo="tilltnet/egonetR")


Example
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