# Phase 2 — Segment geometry: extract_segments()
# Reference: ggdendro::dendro_data(hc, type = "rectangle")$segment()
# Gate: numerical equality to ggdendro on tiny4, medium, and full USArrests.

skip_if_not_installed("ggdendro")

# Helper: ggdendro reference segments for a dataset.
ref_segs <- function(data, method = "complete") {
  hc    <- stats::hclust(stats::dist(data), method = method)
  ddata <- ggdendro::dendro_data(hc, type = "rectangle")
  ggdendro::segment(ddata)
}

# Helper: sort a segments data.frame for order-independent comparison.
sort_segs <- function(s) {
  s[order(s$x, s$y, s$xend, s$yend), ]
}

# --- Structure -----------------------------------------------------------

test_that("extract_segments() returns a data.frame", {
  hc <- hclust_complete(dist(tiny4))
  expect_s3_class(extract_segments(hc), "data.frame")
})

test_that("extract_segments() has columns x, y, xend, yend", {
  hc   <- hclust_complete(dist(tiny4))
  segs <- extract_segments(hc)
  expect_named(segs, c("x", "y", "xend", "yend"))
})

# --- Row count invariant: 4 * (N - 1) ------------------------------------

test_that("extract_segments() row count is 4*(N-1) for tiny4 (N=4)", {
  hc <- hclust_complete(dist(tiny4))
  expect_equal(nrow(extract_segments(hc)), 4 * (nrow(tiny4) - 1))  # 12
})

test_that("extract_segments() row count is 4*(N-1) for medium (N=10)", {
  hc <- hclust_complete(dist(medium))
  expect_equal(nrow(extract_segments(hc)), 4 * (nrow(medium) - 1))  # 36
})

test_that("extract_segments() row count matches ggdendro for full USArrests (N=50)", {
  hc   <- stats::hclust(stats::dist(full), method = "ave")
  segs <- extract_segments(hc)
  ref  <- ref_segs(full, method = "ave")
  expect_equal(nrow(segs), nrow(ref))  # 196
  expect_equal(nrow(segs), 196L)
})

# --- Numerical equality vs ggdendro --------------------------------------

test_that("extract_segments() matches ggdendro exactly [tiny4]", {
  hc   <- hclust_complete(dist(tiny4))
  segs <- sort_segs(extract_segments(hc))
  ref  <- sort_segs(ref_segs(tiny4))
  expect_equal(segs, ref, tolerance = 1e-9)
})

test_that("extract_segments() matches ggdendro exactly [medium, N=10]", {
  hc   <- hclust_complete(dist(medium))
  segs <- sort_segs(extract_segments(hc))
  ref  <- sort_segs(ref_segs(medium))
  expect_equal(segs, ref, tolerance = 1e-9)
})

test_that("extract_segments() matches ggdendro exactly [full USArrests, ave linkage]", {
  hc   <- stats::hclust(stats::dist(full), method = "ave")
  segs <- sort_segs(extract_segments(hc))
  ref  <- sort_segs(ref_segs(full, method = "ave"))
  expect_equal(segs, ref, tolerance = 1e-9)
})

# --- Geometric invariants ------------------------------------------------

test_that("all coordinate values are finite", {
  hc   <- hclust_complete(dist(medium))
  segs <- extract_segments(hc)
  expect_true(all(is.finite(unlist(segs))))
})

test_that("leaf segments touch y = 0", {
  hc        <- hclust_complete(dist(medium))
  segs      <- extract_segments(hc)
  leaf_rows <- segs[segs$y == 0 | segs$yend == 0, ]
  expect_gt(nrow(leaf_rows), 0)
})

test_that("all y values are non-negative", {
  hc   <- hclust_complete(dist(medium))
  segs <- extract_segments(hc)
  expect_true(all(segs$y    >= 0))
  expect_true(all(segs$yend >= 0))
})

test_that("x range spans 1 to N leaves", {
  hc   <- hclust_complete(dist(medium))
  segs <- extract_segments(hc)
  expect_equal(min(c(segs$x, segs$xend)), 1)
  expect_equal(max(c(segs$x, segs$xend)), nrow(medium))
})
