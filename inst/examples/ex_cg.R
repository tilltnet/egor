data("egor32")

# Simplify networks to clustered graphs, stored as igraph objects
graphs <- clustered_graphs(egor32, "country") 

# Visualise
par(mfrow = c(2,3))
vis_clustered_graphs(
  graphs[1:5]
)
par(mfrow = c(1,1))
