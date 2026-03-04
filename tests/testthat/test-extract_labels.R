# Phase 3 — Label extraction: extract_labels()
# Reference: ggdendro::dendro_data(hc, type = "rectangle")$label()
# Gate: x-positions and left-to-right label order match ggdendro.

skip_if_not_installed("ggdendro")

# Helper: ggdendro reference labels.
ref_labels <- function(data, method = "complete") {
  hc    <- stats::hclust(stats::dist(data), method = method)
  ddata <- ggdendro::dendro_data(hc, type = "rectangle")
  ggdendro::label(ddata)  # data.frame(x, y, label)
}

# --- Structure -----------------------------------------------------------

test_that("extract_labels() returns a data.frame", {
  hc <- hclust_complete(dist(tiny4))
  expect_s3_class(extract_labels(hc), "data.frame")
})

test_that("extract_labels() has columns x, y, label", {
  hc  <- hclust_complete(dist(tiny4))
  lbs <- extract_labels(hc)
  expect_true(all(c("x", "y", "label") %in% names(lbs)))
})

test_that("extract_labels() has exactly N rows", {
  for (dat in list(tiny4, medium)) {
    hc  <- hclust_complete(dist(dat))
    lbs <- extract_labels(hc)
    expect_equal(nrow(lbs), nrow(dat), label = paste("N =", nrow(dat)))
  }
})

# --- Leaf y-values are 0 -------------------------------------------------

test_that("all leaf y-values are 0", {
  hc  <- hclust_complete(dist(medium))
  lbs <- extract_labels(hc)
  expect_true(all(lbs$y == 0))
})

# --- Numerical equality vs ggdendro --------------------------------------

test_that("extract_labels() x-positions match ggdendro [tiny4]", {
  hc      <- hclust_complete(dist(tiny4))
  lbs     <- extract_labels(hc)
  ref     <- ref_labels(tiny4)
  merged  <- merge(lbs, ref, by = "label", suffixes = c(".ours", ".ref"))
  expect_equal(merged$x.ours, merged$x.ref, tolerance = 1e-9)
})

test_that("extract_labels() x-positions match ggdendro [medium]", {
  hc      <- hclust_complete(dist(medium))
  lbs     <- extract_labels(hc)
  ref     <- ref_labels(medium)
  merged  <- merge(lbs, ref, by = "label", suffixes = c(".ours", ".ref"))
  expect_equal(merged$x.ours, merged$x.ref, tolerance = 1e-9)
})

test_that("extract_labels() left-to-right order matches ggdendro [medium]", {
  hc        <- hclust_complete(dist(medium))
  lbs       <- extract_labels(hc)
  ref       <- ref_labels(medium)
  our_order <- lbs$label[order(lbs$x)]
  ref_order <- ref$label[order(ref$x)]
  expect_equal(our_order, ref_order)
})

test_that("extract_labels() left-to-right order matches ggdendro [full, ave]", {
  hc        <- stats::hclust(stats::dist(full), method = "ave")
  lbs       <- extract_labels(hc)
  ref       <- ref_labels(full, method = "ave")
  our_order <- lbs$label[order(lbs$x)]
  ref_order <- ref$label[order(ref$x)]
  expect_equal(our_order, ref_order)
})

# --- Custom labels argument ----------------------------------------------

test_that("extract_labels() respects a custom labels argument", {
  hc      <- hclust_complete(dist(tiny4))
  custom  <- c("W", "X", "Y", "Z")
  lbs     <- extract_labels(hc, labels = custom)
  expect_setequal(lbs$label, custom)
})

test_that("extract_labels() errors if custom labels length mismatches N", {
  hc <- hclust_complete(dist(tiny4))
  expect_error(extract_labels(hc, labels = c("A", "B")))
})

# --- Label content when no override ---------------------------------------

test_that("extract_labels() uses hc$labels when no override is given", {
  hc  <- hclust_complete(dist(medium))
  lbs <- extract_labels(hc)
  expect_setequal(lbs$label, rownames(medium))
})
