data("egor32")

# Simplify networks to clustered graphs, stored as igraph objects
graphs <- clustered_graphs(egor32, "alter.age") 

# Visualise
vis_clustered_graphs(graphs)
