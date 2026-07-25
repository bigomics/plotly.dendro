.clade_fixture <- function(max_leaves = 20, n = 100, seed = 1) {
  set.seed(seed)
  hc <- stats::hclust(stats::dist(matrix(stats::rnorm(n * 4), ncol = 4)))
  dendro_cut(dendro_data(hc, hang = 0.03), max_leaves = max_leaves)
}

.built_traces <- function(p) plotly::plotly_build(p)$x$data

test_that("add_dendro_clades() requires a dendro_cut() result", {
  d <- dendro_data(stats::hclust(stats::dist(matrix(stats::rnorm(40), ncol = 4))))
  expect_error(
    add_dendro_clades(plotly::plot_ly(), d),
    "dendro_cut\\(\\) result"
  )
})

test_that("clades sharing a fill colour collapse into a single trace", {
  d <- .clade_fixture()

  one <- .built_traces(add_dendro_clades(plotly::plot_ly(), d))
  expect_length(one, 1L)

  # Three colours over twenty clades must still yield exactly three traces.
  three <- add_dendro_clades(
    plotly::plot_ly(), d,
    fill = rep(c("#ff0000", "#00ff00", "#0000ff"), length.out = nrow(d$clades))
  )
  expect_length(.built_traces(three), 3L)
})

test_that("each wedge is an NA-separated closed polygon", {
  d <- .clade_fixture()
  tr <- .built_traces(add_dendro_clades(plotly::plot_ly(), d))[[1L]]

  expect_identical(tr$fill, "toself")
  expect_false(isTRUE(tr$connectgaps))

  # Four vertices per clade plus one separator between adjacent clades.
  expect_length(tr$x, nrow(d$clades) * 5L - 1L)
  expect_equal(sum(is.na(tr$x)), nrow(d$clades) - 1L)

  # Apex sits at the midpoint of the span, so the wedge is isosceles.
  first <- tr$x[1:4]
  expect_equal(first[3L], mean(c(first[1L], first[2L])))
  expect_equal(first[4L], first[1L])
})

test_that("wedge bases span the clade's true leaf range", {
  d <- .clade_fixture()
  tr <- .built_traces(add_dendro_clades(plotly::plot_ly(), d))[[1L]]

  lo <- tr$x[seq(1L, length(tr$x), by = 5L)]
  hi <- tr$x[seq(2L, length(tr$x), by = 5L)]
  expect_equal(lo, d$clades$span_lo - 0.5)
  expect_equal(hi, d$clades$span_hi + 0.5)

  apex_y <- tr$y[seq(3L, length(tr$y), by = 5L)]
  expect_equal(apex_y, d$clades$height)
})

test_that("shape='none' is a no-op and shape='stub' emits one trace", {
  d <- .clade_fixture()
  p <- plotly::plot_ly()

  expect_identical(add_dendro_clades(p, d, shape = "none"), p)

  stubs <- .built_traces(add_dendro_clades(p, d, shape = "stub"))
  expect_length(stubs, 1L)
  expect_null(stubs[[1L]]$fill)
  expect_length(stubs[[1L]]$x, nrow(d$clades) * 3L - 1L)
})

test_that("fill accepts a function of the clades table", {
  d <- .clade_fixture()
  by_size <- function(clades) ifelse(clades$members > 5, "#aa0000", "#0000aa")

  expect_length(.built_traces(add_dendro_clades(plotly::plot_ly(), d, fill = by_size)), 2L)
})

test_that("add_dendro_clades() rejects a mismatched fill length", {
  d <- .clade_fixture()
  expect_error(
    add_dendro_clades(plotly::plot_ly(), d, fill = c("#ff0000", "#00ff00")),
    "one per clade"
  )
})

test_that("clade glyphs follow the requested orientation", {
  d <- .clade_fixture()
  bottom <- .built_traces(add_dendro_clades(plotly::plot_ly(), d))[[1L]]
  left <- .built_traces(add_dendro_clades(plotly::plot_ly(), d, orientation = "left"))[[1L]]

  # "left" swaps the axes, so the leaf-axis extent moves from x to y.
  expect_equal(left$y, bottom$x)
  expect_equal(left$x, bottom$y)
})
