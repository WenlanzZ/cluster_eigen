library("eigencluster")
library("igraph")
# Tests for cluster_eigen default settting
# --------------------------------------------
context("Valid kopt input")

g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1, 6, 1, 11, 6, 11))

res1 <- cluster_eigen(g)
res2 <- cluster_eigen(g, kopt = 3)
res3 <- cluster_eigen(g, kopt = 3, tune = "fast")

test_that("invalid kopt input message returned", {
  expect_error(
    cluster_eigen(g, kopt = 4), regexp = "larger")
})

context("Valid cluster_eigen output")

test_that("output result", {
  expect_true(inherits(res1, "tbl"))
  expect_true(inherits(res2, "tbl"))
  expect_equal(res1$k[1], 3)
  expect_equal(res2$k, 3)
  expect_equal(res1$k_up[1], 3)
  expect_equal(res2$k_up, 3)
  expect_true(all(res1$cluster[[1]] == res2$cluster[[1]]))
  expect_equivalent(res1$modularity[1], res2$modularity, tolerance = 5e-2)
  expect_equivalent(res1$modularity_up[1], res2$modularity_up, tolerance = 5e-2)
})

test_that("output result", {
    expect_true(inherits(res2, "tbl"))
    expect_equal(res2$k, 3)
    expect_equal(cluster_eigen(g, kopt = 2)$k, 2)
    expect_equal(cluster_eigen(g, kopt = 1)$k, 1)
    expect_equal(res2$k_up, 3)
    expect_equal(cluster_eigen(g, kopt = 1)$cluster[[1]]$mem_up[1], 1)
})
