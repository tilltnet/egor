data("egor32")

# Simplify networks to clustered graphs, stored as igraph objects
graphs <- clustered_graphs(egor32, "country") 

# Visualise
vis_clustered_graphs(graphs, 
                     node.size.multiplier = 5, 
                     edge.width.multiplier = 25,
                     labels = TRUE)
