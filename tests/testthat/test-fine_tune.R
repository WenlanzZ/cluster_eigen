library("eigencluster")
library("igraph")
# Tests for fine_tune default settting
# --------------------------------------------
context("output result")

g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1, 6, 1, 11, 6, 11))

n        <- vcount(g)
eclidean <- diag(1, n) - as.matrix(as_adjacency_matrix(g))
mod      <- modularity_matrix(g, seq_len(length(V(g))))
w <- eigen(mod)
lambda   <- w$values
u        <- w$vectors
dim <- sum(lambda > 0)

r <- u[, seq_len(dim)] %*% diag(sqrt(lambda[seq_len(dim)]))
label <- c(rep(1,4), 2, rep(2, 4), 3, rep(3, 5))
res <- fine_tune(label, r)

test_that("output result", {
    expect_true(inherits(res, "tbl"))
})
