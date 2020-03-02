# egor 0.20.03

- fix: updated clustered_graphs() to work with tibble 3.0.0

# egor 0.20.01

- feat: added count_dyads() function
- feat: plot_ego_gram now uses plot_ego_graph for graph plotting
- feat: added ego_constraint
- fix: significantly sped up trim_aaties; hence most deplyr methods work much faster especially on big datasets
- several fixes and improvements to plotting, importing and infrastructure