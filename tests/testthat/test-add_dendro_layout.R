# add_dendro_layout() — layout-only composition point
# Reference: C2 composability design — layout without adding traces.
# Gate: applies axis config to plotly; does NOT add data traces.

skip_if_not_installed("plotly")

test_that("add_dendro_layout() returns a plotly", {
  d <- dendro_data(tiny4)
  p <- add_dendro_layout(plotly::plot_ly(), d)
  expect_s3_class(p, "plotly")
})

test_that("add_dendro_layout() sets tickvals on the leaf axis", {
  d <- dendro_data(tiny4)
  p <- add_dendro_layout(plotly::plot_ly(), d, orientation = "bottom")
  built <- plotly::plotly_build(p)
  expect_false(is.null(built$x$layout$xaxis$tickvals))
  expect_length(built$x$layout$xaxis$tickvals, nrow(tiny4))
})

test_that("add_dendro_layout() with show_labels=FALSE omits tickvals and suppresses auto-ticks", {
  d <- dendro_data(tiny4)
  p <- add_dendro_layout(plotly::plot_ly(), d, show_labels = FALSE)
  built <- plotly::plotly_build(p)
  expect_null(built$x$layout$xaxis$tickvals)
  expect_null(built$x$layout$xaxis$ticktext)
  expect_false(isTRUE(built$x$layout$xaxis$showticklabels))
})

test_that("add_dendro_layout() sets ticks='' on both axes", {
  d <- dendro_data(tiny4)
  p <- add_dendro_layout(plotly::plot_ly(), d)
  built <- plotly::plotly_build(p)
  expect_equal(built$x$layout$xaxis$ticks, "")
  expect_equal(built$x$layout$yaxis$ticks, "")
})

test_that("add_dendro_layout() does NOT add data traces", {
  d <- dendro_data(tiny4)
  p <- add_dendro_layout(plotly::plot_ly(), d)
  built <- plotly::plotly_build(p)
  # plot_ly() creates one empty trace by default; no additional traces
  expect_true(length(built$x$data) <= 1)
})

test_that("add_dendro_layout() works with horizontal orientation", {
  d <- dendro_data(tiny4)
  p <- add_dendro_layout(plotly::plot_ly(), d, orientation = "left")
  built <- plotly::plotly_build(p)
  expect_false(is.null(built$x$layout$yaxis$tickvals))
  expect_null(built$x$layout$xaxis$tickvals)
})

test_that("add_dendro_layout() show_labels=FALSE works with horizontal orientation", {
  d <- dendro_data(tiny4)
  p <- add_dendro_layout(plotly::plot_ly(), d, orientation = "left", show_labels = FALSE)
  built <- plotly::plotly_build(p)
  expect_null(built$x$layout$yaxis$tickvals)
  expect_false(isTRUE(built$x$layout$yaxis$showticklabels))
})
