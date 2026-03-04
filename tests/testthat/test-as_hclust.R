# Phase 1 — Input coercion: as_hclust()
# Reference: R contract only (no external reference).
# Gate: as_hclust() accepts matrix, dist, hclust; rejects everything else.

test_that("as_hclust() accepts a numeric matrix", {
  hc <- as_hclust(tiny4)
  expect_s3_class(hc, "hclust")
  expect_equal(sort(hc$labels), sort(rownames(tiny4)))
})

test_that("as_hclust() accepts a data.frame", {
  hc <- as_hclust(medium)
  expect_s3_class(hc, "hclust")
  expect_equal(length(hc$order), nrow(medium))
})

test_that("as_hclust() accepts a dist object", {
  d  <- dist(tiny4)
  hc <- as_hclust(d)
  expect_s3_class(hc, "hclust")
  # must equal manual hclust(dist) on the same input
  ref <- stats::hclust(d)
  expect_equal(hc$merge,  ref$merge)
  expect_equal(hc$order,  ref$order)
  expect_equal(hc$height, ref$height)
})

test_that("as_hclust() is identity on an hclust object", {
  hc  <- stats::hclust(dist(tiny4))
  hc2 <- as_hclust(hc)
  expect_identical(hc, hc2)
})

test_that("as_hclust() produces N leaves for N-row input", {
  for (dat in list(tiny4, medium)) {
    hc <- as_hclust(dat)
    expect_equal(length(hc$order), nrow(dat))
  }
})

test_that("as_hclust() rejects a character vector", {
  expect_error(as_hclust(c("a", "b", "c")))
})

test_that("as_hclust() rejects a plain list", {
  expect_error(as_hclust(list(x = 1, y = 2)))
})

test_that("as_hclust() rejects NULL", {
  expect_error(as_hclust(NULL))
})
