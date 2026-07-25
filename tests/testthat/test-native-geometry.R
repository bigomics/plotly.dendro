skip_if_not_installed("dendextend")

.expect_native_matches_dendextend <- function(hc, hang = NULL, labels = NULL) {
  dend <- if (is.null(hang)) {
    stats::as.dendrogram(hc)
  } else {
    stats::as.dendrogram(hc, hang = hang)
  }
  reference <- dendextend::as.ggdend(dend)
  observed <- dendro_data(hc, labels = labels, hang = hang)

  for (column in c("x", "y", "xend", "yend")) {
    expect_equal(
      observed$segments[[column]],
      reference$segments[[column]],
      tolerance = 0
    )
  }
  expect_equal(observed$labels$x, reference$labels$x, tolerance = 0)
  expect_equal(observed$labels$y, reference$labels$y, tolerance = 0)
  if (is.null(labels)) {
    expect_identical(
      observed$labels$label,
      as.character(reference$labels$label)
    )
  }
  invisible(observed)
}

test_that("native geometry matches base/dendextend on two-leaf and tree shapes", {
  two <- stats::hclust(stats::dist(matrix(c(0, 1), ncol = 1)))
  two$labels <- c("A", "B")

  for (hc in list(two, .balanced_hclust(), .comb_hclust(64L))) {
    .expect_native_matches_dendextend(hc)
    expect_equal(nrow(dendro_data(hc)$segments), 4L * length(hc$height))
  }
})

test_that("native geometry preserves tied heights and tied distances", {
  tied_distances <- stats::hclust(stats::dist(rbind(
    c(0, 0), c(0, 0), c(1, 1), c(1, 1), c(2, 2), c(2, 2)
  )), method = "complete")
  tied_heights <- .balanced_hclust()

  .expect_native_matches_dendextend(tied_distances)
  .expect_native_matches_dendextend(tied_heights)
})

test_that("R validation normalizes integer-valued numeric hclust fields", {
  hc <- .balanced_hclust()
  expected <- dendro_data(hc)

  storage.mode(hc$merge) <- "double"
  hc$order <- as.numeric(hc$order)
  observed <- dendro_data(hc)

  expect_identical(observed$segments, expected$segments)
  expect_identical(observed$labels, expected$labels)
  expect_identical(observed$nodes, expected$nodes)

  hc$merge[1L, 1L] <- -1.5
  expect_error(dendro_data(hc), "finite integer values")
})

test_that("hang uses the final merge height on non-monotone trees", {
  set.seed(20260724)
  x <- matrix(rnorm(12L * 5L), nrow = 12L)

  for (method in c("centroid", "median")) {
    hc <- stats::hclust(stats::dist(x), method = method)
    if (max(hc$height) == tail(hc$height, 1L)) {
      hc$height[length(hc$height) - 1L] <- tail(hc$height, 1L) * 1.25
    }
    expect_gt(max(hc$height), tail(hc$height, 1L))
    .expect_native_matches_dendextend(hc, hang = 0.03)
  }
})

test_that("native geometry matches all selected hang modes", {
  hc <- .comb_hclust(12L)
  for (hang in list(NULL, -1, 0, 0.03, 1000)) {
    .expect_native_matches_dendextend(hc, hang = hang)
  }
})

test_that("native labels support absent, duplicate, and custom labels", {
  hc <- .balanced_hclust()
  hc$labels <- NULL
  absent <- dendro_data(hc)
  expect_identical(
    absent$labels$label,
    as.character(seq_len(8L)[hc$order])
  )

  hc$labels <- rep(c("same", "other"), 4L)
  duplicate <- .expect_native_matches_dendextend(hc)
  expect_identical(
    duplicate$labels$label,
    as.character(hc$labels[hc$order])
  )

  custom <- paste0("custom-", seq_len(8L))
  observed <- dendro_data(hc, labels = custom)
  expect_identical(observed$labels$label, custom[hc$order])
  expect_error(
    dendro_data(hc, labels = custom[-1L]),
    "one entry per leaf"
  )
})

test_that("nodes default on and opt out with a typed empty schema", {
  hc <- .balanced_hclust()
  with_nodes <- dendro_data(hc)
  expect_equal(nrow(with_nodes$nodes), length(hc$height))
  expect_type(with_nodes$nodes$x, "double")
  expect_type(with_nodes$nodes$y, "double")
  expect_type(with_nodes$nodes$members, "integer")
  expect_type(with_nodes$nodes$height, "double")

  without_nodes <- dendro_data(hc, nodes = FALSE)
  expect_identical(without_nodes$nodes, .empty_nodes())
  expect_error(dendro_data(hc, nodes = NA), "nodes must be TRUE or FALSE")

  plotted <- plot_dendro(hc)
  expect_identical(attr(plotted, "dendro_data")$nodes, .empty_nodes())
})

test_that("node output preserves legacy unique-coordinate ordering", {
  hc <- .balanced_hclust()
  hc$height[1:4] <- 1
  observed <- dendro_data(hc)$nodes
  segs <- dendro_data(hc, nodes = FALSE)$segments
  horizontal <- segs[
    segs$y == segs$yend & segs$x != segs$xend,
    c("x", "y"),
    drop = FALSE
  ]
  expected <- unique(horizontal)
  expected <- expected[order(expected$y, expected$x), , drop = FALSE]
  rownames(expected) <- NULL

  expect_identical(observed$x, expected$x)
  expect_identical(observed$y, expected$y)
  expect_identical(observed$height, expected$y)
  expect_identical(observed$members, rep.int(NA_integer_, nrow(expected)))
})

test_that("all orientations preserve exact native coordinate transforms", {
  d <- dendro_data(.balanced_hclust(), hang = 0.03, nodes = FALSE)
  for (orientation in c("bottom", "top", "left", "right")) {
    observed <- orient_data(d, orientation)
    expected <- lapply(
      d[c("segments", "labels", "nodes")],
      .orient_xy,
      orientation = orientation
    )
    expect_identical(observed$segments, expected$segments)
    expect_identical(observed$labels, expected$labels)
    expect_identical(observed$nodes, expected$nodes)
  }
})

test_that("native validation rejects malformed hclust safely", {
  hc <- .balanced_hclust()
  original <- unserialize(serialize(hc, NULL))

  malformed <- hc
  malformed$merge[1L, 1L] <- 0L
  expect_error(dendro_data(malformed), "non-zero")

  malformed <- hc
  malformed$merge[2L, 1L] <- 2L
  expect_error(dendro_data(malformed), "earlier rows")

  malformed <- hc
  malformed$merge[1L, 1L] <- -99L
  expect_error(dendro_data(malformed), "out of range")

  malformed <- hc
  malformed$order[2L] <- malformed$order[1L]
  expect_error(dendro_data(malformed), "duplicate")

  malformed <- hc
  malformed$order <- rev(malformed$order)
  expect_error(dendro_data(malformed), "inconsistent")

  malformed <- hc
  malformed$height[1L] <- NA_real_
  expect_error(dendro_data(malformed), "finite")

  malformed <- hc
  malformed$merge[7L, 1L] <- 6L
  expect_error(dendro_data(malformed), "exactly one parent")

  expect_identical(hc, original)
})

test_that("native calls never mutate input hclust vectors", {
  hc <- .comb_hclust(100L)
  original <- unserialize(serialize(hc, NULL))
  invisible(dendro_data(hc, hang = 0.03))
  expect_identical(hc, original)
})

test_that("iterative native traversal handles a deep 10,000-leaf comb", {
  hc <- .comb_hclust(10000L)
  observed <- dendro_data(hc, hang = 0.03, nodes = FALSE)
  expect_equal(nrow(observed$segments), 4L * (10000L - 1L))
  expect_equal(nrow(observed$labels), 10000L)
})
