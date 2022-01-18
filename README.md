[![CRAN status](https://www.r-pkg.org/badges/version/egor)](https://CRAN.R-project.org/package=egor)
[![R-CMD-check](https://github.com/tilltnet/egor/workflows/R-CMD-check/badge.svg)](https://github.com/tilltnet/egor/actions)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tilltnet/egor?branch=master&svg=true)](https://ci.appveyor.com/project/tilltnet/egor)

# egor

Tools for importing, analyzing and visualizing ego-centered or personal network
data. egor integrates nicely with the [tidyverse](http://www.tidyverse.org), by
providing methods for most `dplyr` commands. An `egor` object contains the three
data levels, `ego`, `alter` and alter-alter ties (`aatie`). In order to switch
between the three data levels the `activate()` concept is lent from the
[tidygraph](https://github.com/thomasp85/tidygraph) package.

## Installation

    # Install release version from CRAN
    install.packages("egor")

    # Install development version from GitHub
    remotes::install_github(repo="tilltnet/egor")
    
## Visualize

`egor` offers a few visualization techniques for ego-centered network data:

- regular network plots ![](https://www.tillt.net/egor/reference/figures/network_plot.png)
- clustered graphs ![](https://www.tillt.net/egor/reference/figures/clustered_graphs.png)
- ego-grams ![](https://www.tillt.net/egor/reference/figures/ego_gram.png)

And there is a Network Visualization App providing a graphical interface that
let's the user compose their visualizations interactively. 

![](https://www.tillt.net/egor/reference/figures/vis_wizzard.png)

## Import data

An egor object can be created with the `egor()` command. In addition there are
some functions that import specific data formats for ego-centered network data.

There are currently three importing functions that read the data exported from
data collection tools from the hard drive and load them as an `egor` object.

```
read_openeddi()
read_egoweb()
read_egonet()
```

In addition there are three functions that help with the transformation of common data 
formats of ego-centered network data into egor objects:

```
onefile_to_egor()
twofiles_to_egor()
threefiles_to_egor()
```

## Analyse

There are a few commands facilitating the analysis of ego-centered networks. To
learn more, please take a look at the package 
[vignette](articles/using_egor.html).
