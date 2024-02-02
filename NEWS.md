# egor 1.22.13

- added method for `dplyr::distinct()` (#63)
- added methods for `dplyr`'s `slice_head()`, `slice_tail()`, `slice_min()`, `slice_max()`, and `slice_sample()` (#84)
- fixed doc typos

# egor 1.22.13

- fixed igraph issue (#82)

# egor 1.22.12

- fixed printing issues (#80)
- fixed layout issues for `clustered_graphs()` and `plot_egograms()`
- fixed `tidyselect` deprecation warnings

# egor 1.22.3

- dropped AppVeyor CI
- some house keeping

# egor 1.22.1

- bug fixes

# egor 1.21.6

- added vignette for ego-centered network surveys in Qualtrics
- `as.egor()` now can convert a list of `igraph` or `network` objects to an `egor` object
- improved `plot_egograms()` consistency
- `as_network()` handles alter-alter weights more consistently when `include.ego = TRUE`

# egor 0.21.01

- changed printing behavior of `egor` objects
- `EI()` re-scaling can be turned off and ego can be included in EI calculation
- improved `plot_egograms()` (i.e. node size scaling)
- `dplyr::rowwise()` is now supported
- new example data `transnat` and `gss2004`
- ego-alter weights are now plotted correctly by `plot_egographs()`
- several bugs fixed

# egor 0.20.06

- we are now using the `srvyr` package to allow for survey designs being reflected in the `egor` object
- adjustments to work with dplyr 1.0.0
- feat: Added as.egor.nested_egor()
- feat: added rowwise_egor()

# egor 0.20.03

- fix: updated clustered_graphs() to work with tibble 3.0.0

# egor 0.20.01

- feat: added count_dyads() function
- feat: plot_ego_gram now uses plot_ego_graph for graph plotting
- feat: added ego_constraint
- fix: significantly sped up trim_aaties; hence most dplyr methods work much faster especially on big datasets
- several fixes and improvements to plotting, importing and infrastructure