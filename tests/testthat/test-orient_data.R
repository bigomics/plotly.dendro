# Phase 5 — Orientation transforms: orient_data()
# Reference: analytic sign-flip invariants (no external reference).
# Gate: all four orientations satisfy the coordinate algebra from R_api_design.R.
#
# Sign table:
#   "bottom" → identity
#   "top"    → negate y (and yend)
#   "left"   → swap x↔y  (x=old_y, y=old_x, xend=old_yend, yend=old_xend)
#   "right"  → swap x↔y then negate new y

make_dendro_list <- function(data, method = "complete") {
  hc <- stats::hclust(stats::dist(data), method = method)
  list(
    segments = extract_segments(hc),
    labels   = extract_labels(hc),
    nodes    = extract_nodes(hc)
  )
}

# --- "bottom" is the identity --------------------------------------------

test_that("orient_data('bottom') leaves segments unchanged", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "bottom")
  expect_equal(out$segments, d$segments)
})

test_that("orient_data('bottom') leaves labels unchanged", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "bottom")
  expect_equal(out$labels, d$labels)
})

test_that("orient_data('bottom') leaves nodes unchanged", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "bottom")
  expect_equal(out$nodes, d$nodes)
})

# --- "top": negate y-axis ------------------------------------------------

test_that("orient_data('top') keeps x unchanged and negates y [segments]", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "top")
  expect_equal(out$segments$x,     d$segments$x)
  expect_equal(out$segments$xend,  d$segments$xend)
  expect_equal(out$segments$y,    -d$segments$y)
  expect_equal(out$segments$yend, -d$segments$yend)
})

test_that("orient_data('top') negates y for labels", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "top")
  expect_equal(out$labels$x, d$labels$x)
  expect_equal(out$labels$y, -d$labels$y)
})

# --- "left": swap x↔y ----------------------------------------------------

test_that("orient_data('left') swaps x and y in segments", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "left")
  expect_equal(out$segments$x,    d$segments$y)
  expect_equal(out$segments$y,    d$segments$x)
  expect_equal(out$segments$xend, d$segments$yend)
  expect_equal(out$segments$yend, d$segments$xend)
})

test_that("orient_data('left') swaps x and y in labels", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "left")
  expect_equal(out$labels$x, d$labels$y)
  expect_equal(out$labels$y, d$labels$x)
})

test_that("orient_data('left') swaps x and y in nodes", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "left")
  expect_equal(out$nodes$x, d$nodes$y)
  expect_equal(out$nodes$y, d$nodes$x)
})

# --- "right": swap x↔y then negate new y ---------------------------------

test_that("orient_data('right') swaps and negates in segments", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "right")
  expect_equal(out$segments$x,     d$segments$y)
  expect_equal(out$segments$y,    -d$segments$x)
  expect_equal(out$segments$xend,  d$segments$yend)
  expect_equal(out$segments$yend, -d$segments$xend)
})

test_that("orient_data('right') swaps and negates in labels", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "right")
  expect_equal(out$labels$x, d$labels$y)
  expect_equal(out$labels$y, -d$labels$x)
})

# --- Double application is symmetric: left∘left = bottom -----------------

test_that("applying 'left' twice returns to 'bottom' layout (x-range)", {
  d    <- make_dendro_list(tiny4)
  once <- orient_data(d, "left")
  twice <- orient_data(once, "left")
  # x of twice == x of bottom (modulo sign since swap∘swap = identity)
  expect_equal(abs(twice$segments$x),    abs(d$segments$x))
  expect_equal(abs(twice$segments$xend), abs(d$segments$xend))
})

# --- Preserves list structure --------------------------------------------

test_that("orient_data() returns a list with segments, labels, nodes", {
  d   <- make_dendro_list(tiny4)
  out <- orient_data(d, "bottom")
  expect_named(out, c("segments", "labels", "nodes"), ignore.order = TRUE)
})

# --- Invalid orientation -------------------------------------------------

test_that("orient_data() rejects an invalid orientation string", {
  d <- make_dendro_list(tiny4)
  expect_error(orient_data(d, "diagonal"))
  expect_error(orient_data(d, ""))
})
