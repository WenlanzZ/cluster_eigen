library(roxygen2)
library(devtools)
setwd("/Users/wz262/Projects/eigencluster")
document()
install()
3
library(eigencluster)

load_all()
library(igraph)
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
cluster_louvain(g)$membership
fast_tune(cluster_louvain(g)$membership, r)
res <- cluster_eigen(g, kopt = 3)
cluster_eigen(g, kopt = 2)$cluster
cluster_eigen(g, kopt = 1)$cluster
plot(g, vertex.color = res$cluster[[1]]$label_up)


load_all()
devtools::test()
devtools::check(vignettes = FALSE)
?cluster_eigen
?fast_tune
?subspace
?dimension
?truncate
?lung
#lintr
setwd("/Users/wz262/Projects/eigencluster")
library(devtools)
a <- lint(".", cache = FALSE)
library(dplyr)
a %>% as_tibble()
library(covr)
a <- covr::report()
a