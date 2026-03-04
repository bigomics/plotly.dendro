# dendro_data() — public orchestrator: build tidy dendrogram data
# Reference: plotly R class contract c("plotly", "htmlwidget").
# Gate: returns correct list structure; accepts all input types; respects distfun/linkagefun.

skip_if_not_installed("plotly")

test_that("dendro_data() returns a list", {
  expect_type(dendro_data(tiny4), "list")
})

test_that("dendro_data() has segments, labels, nodes", {
  d <- dendro_data(tiny4)
  expect_named(d, c("segments", "labels", "nodes"), ignore.order = TRUE)
})

test_that("dendro_data()$segments is a data.frame with x,y,xend,yend", {
  d <- dendro_data(tiny4)
  expect_s3_class(d$segments, "data.frame")
  expect_named(d$segments, c("x", "y", "xend", "yend"))
})

test_that("dendro_data()$labels is a data.frame with x, y, label", {
  d <- dendro_data(tiny4)
  expect_s3_class(d$labels, "data.frame")
  expect_true(all(c("x", "y", "label") %in% names(d$labels)))
})

test_that("dendro_data()$nodes is a data.frame", {
  d <- dendro_data(tiny4)
  expect_s3_class(d$nodes, "data.frame")
})

test_that("dendro_data() accepts matrix, dist, and hclust — same segments", {
  d_mat  <- dendro_data(tiny4)
  d_dist <- dendro_data(dist(tiny4))
  d_hc   <- dendro_data(hclust_complete(dist(tiny4)))
  expect_equal(d_mat$segments,  d_dist$segments)
  expect_equal(d_mat$segments,  d_hc$segments)
  expect_equal(d_mat$labels,    d_dist$labels)
  expect_equal(d_mat$labels,    d_hc$labels)
})

test_that("dendro_data() custom linkagefun is respected", {
  single <- dendro_data(tiny4, linkagefun = function(d) stats::hclust(d, method = "single"))
  complete <- dendro_data(tiny4, linkagefun = function(d) stats::hclust(d, method = "complete"))
  # Different linkage → different merge order → different segments
  # (heights will differ even if topology is the same for tiny4)
  expect_false(identical(single$segments$y, complete$segments$y))
})

test_that("dendro_data() custom distfun is respected", {
  eucl    <- dendro_data(medium, distfun = function(x) dist(x, method = "euclidean"))
  maximum <- dendro_data(medium, distfun = function(x) dist(x, method = "maximum"))
  # Different distance metric → different clustering → different segments
  expect_false(identical(eucl$segments, maximum$segments))
})
