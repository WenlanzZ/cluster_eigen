# cluster_eigen

<!-- badges: start -->
<!-- badges: end -->

This function implements a vector partition algorithm with global initialization that maximizes the modilarity measure and provide membership
for community assignment.

## Installation

You can install the released version of dimension from GitHub with:

``` r
devtools::install_github("WenlanzZ/eigencluster")
```

## Example

This is a basic example which shows you how to use this package:

``` r
library(dimension)
library(igraph)
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
res <- cluster_eigen(g)
# color by label and size by membership
plot(g, vertex.color = res$cluster[[1]]$label_up, vertex.size = res$cluster[[1]]$mem_up *4)
```
