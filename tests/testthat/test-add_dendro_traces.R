# add_dendro_traces() — trace-only composition point (no layout side-effects)
# Reference: C2 composability design — traces without layout side-effects.
# Gate: returns plotly with segment traces; does NOT touch layout or attach dendro_data.

skip_if_not_installed("plotly")

test_that("add_dendro_traces() returns a plotly", {
  d <- dendro_data(tiny4)
  p <- add_dendro_traces(plotly::plot_ly(), d)
  expect_s3_class(p, "plotly")
})

test_that("add_dendro_traces() adds segment traces", {
  d <- dendro_data(tiny4)
  p <- add_dendro_traces(plotly::plot_ly(), d)
  built <- plotly::plotly_build(p)
  expect_true(length(built$x$data) >= 1)
})

test_that("add_dendro_traces() does NOT set axis tickvals", {
  d <- dendro_data(tiny4)
  p <- add_dendro_traces(plotly::plot_ly(), d)
  built <- plotly::plotly_build(p)
  expect_null(built$x$layout$xaxis$tickvals)
})

test_that("add_dendro_traces() passes line properties through", {
  d <- dendro_data(tiny4)
  p <- add_dendro_traces(plotly::plot_ly(), d, line = list(color = "green", width = 3, dash = "dot"))
  built <- plotly::plotly_build(p)
  colors <- vapply(built$x$data, function(tr) tr$line$color, character(1))
  expect_true(all(colors == "green"))
  widths <- vapply(built$x$data, function(tr) tr$line$width, numeric(1))
  expect_true(all(widths == 3))
  dashes <- vapply(built$x$data, function(tr) tr$line$dash, character(1))
  expect_true(all(dashes == "dot"))
})

test_that("add_dendro_traces() respects orientation", {
  d <- dendro_data(tiny4)
  p_bottom <- plotly::plotly_build(add_dendro_traces(plotly::plot_ly(), d, orientation = "bottom"))
  p_left   <- plotly::plotly_build(add_dendro_traces(plotly::plot_ly(), d, orientation = "left"))
  # In "left" orientation, x and y swap — data should differ
  expect_false(identical(p_bottom$x$data[[1]]$x, p_left$x$data[[1]]$x))
})

test_that("add_dendro_traces() does NOT attach dendro_data attribute", {
  d <- dendro_data(tiny4)
  p <- add_dendro_traces(plotly::plot_ly(), d)
  expect_null(attr(p, "dendro_data"))
})
