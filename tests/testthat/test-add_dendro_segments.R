# add_dendro_segments() — layer function adding colored segment traces
# Reference: plotly R class contract c("plotly", "htmlwidget").
# Gate: returns a valid plotly object when called on dendro_data() output.

skip_if_not_installed("plotly")

test_that("add_dendro_segments() returns a plotly", {
  hc    <- hclust_complete(dist(tiny4))
  d     <- dendro_data(tiny4)
  p     <- add_dendro_segments(plotly::plot_ly(), d)
  expect_s3_class(p, "plotly")
})

test_that("default line color matches default_colorscale()[1]", {
  d <- dendro_data(tiny4)
  p <- add_dendro_segments(plotly::plot_ly(), d)
  built <- plotly::plotly_build(p)
  colors <- vapply(built$x$data, function(tr) tr$line$color, character(1))
  expect_true(all(colors == default_colorscale()[1]))
})

test_that("line$color overrides default segment color", {
  d <- dendro_data(tiny4)
  p <- add_dendro_segments(plotly::plot_ly(), d, line = list(color = "red"))
  built <- plotly::plotly_build(p)
  colors <- vapply(built$x$data, function(tr) tr$line$color, character(1))
  expect_true(all(colors == "red"))
})

test_that("line$width is applied to all traces", {
  d <- dendro_data(tiny4)
  p <- add_dendro_segments(plotly::plot_ly(), d, line = list(width = 2))
  built <- plotly::plotly_build(p)
  widths <- vapply(built$x$data, function(tr) tr$line$width, numeric(1))
  expect_true(all(widths == 2))
})

test_that("line$dash is applied to all traces", {
  d <- dendro_data(tiny4)
  p <- add_dendro_segments(plotly::plot_ly(), d, line = list(dash = "dot"))
  built <- plotly::plotly_build(p)
  dashes <- vapply(built$x$data, function(tr) tr$line$dash, character(1))
  expect_true(all(dashes == "dot"))
})

test_that("color_threshold overrides line$color but line$width still applies", {
  hc <- hclust_complete(dist(tiny4))
  d <- dendro_data(hc)
  p <- add_dendro_segments(plotly::plot_ly(), d,
                           color_threshold = 3,
                           line = list(color = "red", width = 0.5))
  built <- plotly::plotly_build(p)
  # line$color should be ignored — branch colors used instead
  colors <- vapply(built$x$data, function(tr) tr$line$color, character(1))
  expect_false(all(colors == "red"))
  # line$width should still apply
  widths <- vapply(built$x$data, function(tr) tr$line$width, numeric(1))
  expect_true(all(widths == 0.5))
})

test_that("passing line via ... does not cause duplicate arg error", {
  d <- dendro_data(tiny4)
  # This previously errored with "argument 3 matches multiple formal arguments"
  expect_no_error({
    p <- add_dendro_segments(plotly::plot_ly(), d, line = list(dash = "dot", width = 1.5))
    plotly::plotly_build(p)
  })
})
