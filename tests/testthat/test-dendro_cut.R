.cut_fixture <- function(n = 100, seed = 1) {
  set.seed(seed)
  stats::hclust(stats::dist(matrix(stats::rnorm(n * 4), ncol = 4)))
}

test_that("dendro_cut() returns the input unchanged when the budget covers every leaf", {
  hc <- .cut_fixture()
  d <- dendro_data(hc, hang = 0.03)

  expect_identical(dendro_cut(d, max_leaves = Inf), d)
  expect_identical(dendro_cut(d, max_leaves = length(hc$order)), d)
  expect_identical(dendro_cut(d, max_leaves = length(hc$order) + 10), d)
})

test_that("dendro_cut() honours the budget and the clades = expanded + 1 invariant", {
  d <- dendro_data(.cut_fixture(), hang = 0.03)

  for (budget in c(2, 5, 20, 47)) {
    cut <- dendro_cut(d, max_leaves = budget)
    info <- attr(cut, "cut")
    expect_equal(nrow(cut$clades), budget)
    expect_equal(info$clades, info$expanded + 1L)
  }
})

test_that("the collapsed frontier partitions the leaves exactly once", {
  hc <- .cut_fixture()
  n <- length(hc$order)
  cut <- dendro_cut(dendro_data(hc, hang = 0.03), max_leaves = 20)

  expect_equal(sum(cut$clades$members), n)

  # Spans must tile 1..n with no gap and no overlap.
  ordered <- cut$clades[order(cut$clades$span_lo), , drop = FALSE]
  expect_equal(ordered$span_lo[1L], 1L)
  expect_equal(ordered$span_hi[nrow(ordered)], n)
  expect_equal(ordered$span_hi[-nrow(ordered)] + 1L, ordered$span_lo[-1L])
  expect_equal(ordered$span_hi - ordered$span_lo + 1L, ordered$members)
})

test_that("a smaller budget yields a coarser cut than a larger one", {
  d <- dendro_data(.cut_fixture(), hang = 0.03)
  coarse <- dendro_cut(d, max_leaves = 10)
  fine <- dendro_cut(d, max_leaves = 40)

  expect_lt(nrow(coarse$clades), nrow(fine$clades))
  expect_lt(nrow(coarse$segments), nrow(fine$segments))
  expect_gt(max(coarse$clades$members), max(fine$clades$members))
})

test_that("cut segments stay within the original tree's coordinate envelope", {
  d <- dendro_data(.cut_fixture(), hang = 0.03)
  cut <- dendro_cut(d, max_leaves = 20)

  expect_named(cut$segments, c("x", "y", "xend", "yend"))
  expect_gte(min(cut$segments$x, cut$segments$xend), min(d$segments$x, d$segments$xend))
  expect_lte(max(cut$segments$x, cut$segments$xend), max(d$segments$x, d$segments$xend))
  expect_lte(max(cut$segments$y, cut$segments$yend), max(d$segments$y, d$segments$yend))
})

test_that("built-in priorities differ in how they spend the budget", {
  d <- dendro_data(.cut_fixture(400, seed = 7), hang = 0.03)

  by_members <- dendro_cut(d, max_leaves = 40, priority = "members")$clades
  by_height <- dendro_cut(d, max_leaves = 40, priority = "height")$clades

  # members-priority always splits the biggest clade, so it cannot leave a
  # clade larger than height-priority's worst case.
  expect_lte(max(by_members$members), max(by_height$members))
  expect_equal(sum(by_members$members), sum(by_height$members))
})

test_that("dendro_cut() accepts a custom priority function", {
  d <- dendro_data(.cut_fixture(), hang = 0.03)

  cut <- dendro_cut(
    d,
    max_leaves = 15,
    priority = function(members, height) members * height
  )
  expect_equal(nrow(cut$clades), 15)
  expect_identical(attr(cut, "cut")$priority, "custom")

  expect_error(
    dendro_cut(d, max_leaves = 15, priority = function(members, height) 1),
    "one numeric score per merge"
  )
})

test_that("min_clade stops subdivision once clades fall below the floor", {
  d <- dendro_data(.cut_fixture(400, seed = 3), hang = 0.03)

  unfloored <- dendro_cut(d, max_leaves = 300, min_clade = 1)
  floored <- dendro_cut(d, max_leaves = 300, min_clade = 20)

  # The floor makes the peel stop early rather than exhaust the budget.
  expect_lt(nrow(floored$clades), nrow(unfloored$clades))
  expect_equal(sum(floored$clades$members), sum(unfloored$clades$members))
})

test_that("dendro_cut() rejects malformed input", {
  d <- dendro_data(.cut_fixture(), hang = 0.03)

  expect_error(dendro_cut(list(segments = NULL)), "hclust")
  expect_error(dendro_cut(d, max_leaves = 0), "at least 1")
  expect_error(dendro_cut(d, max_leaves = NA), "single number")
  expect_error(dendro_cut(d, max_leaves = c(1, 2)), "single number")
  expect_error(dendro_cut(d, max_leaves = 10, min_clade = 0), "at least 1")
})

test_that("the native node table matches the R-level tree statistics", {
  hc <- .cut_fixture()
  inputs <- .normalize_hclust_inputs(hc)
  tbl <- node_table(inputs$merge, inputs$height, inputs$order)

  n <- length(hc$order)
  expect_equal(nrow(tbl), n - 1L)
  expect_equal(tbl$height, as.numeric(hc$height))

  # members is monotone: a child never holds more leaves than its parent.
  child_members <- function(k) if (k < 0L) 1L else tbl$members[k]
  for (i in seq_len(nrow(tbl))) {
    expect_lte(child_members(tbl$left[i]), tbl$members[i])
    expect_lte(child_members(tbl$right[i]), tbl$members[i])
  }

  # The root spans every leaf.
  expect_equal(tbl$members[nrow(tbl)], n)
  expect_equal(tbl$span_lo[nrow(tbl)], 1L)
  expect_equal(tbl$span_hi[nrow(tbl)], n)
  expect_equal(tbl$span_hi - tbl$span_lo + 1L, tbl$members)
})
