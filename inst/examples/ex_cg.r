data("egoR32")

# Simplify networks to clustered graphs, stored as igraph objects
graphs <- clustered.graphs(egoR32$alteri.list, egoR32$edges, "alter.age") 

# Visualise
vis.clustered.graphs(graphs)
