# add_dendro() — convenience wrapper adding segments + layout to an existing plotly
# Reference: plotly R class contract c("plotly", "htmlwidget").
# Gate: adds to existing figure; returns valid pipeable plotly object.

skip_if_not_installed("plotly")

test_that("add_dendro() adds to an existing plotly object", {
  base <- plotly::plot_ly()
  p    <- add_dendro(base, tiny4)
  expect_s3_class(p, "plotly")
})

test_that("add_dendro() returns a plotly, not just a list", {
  p <- add_dendro(plotly::plot_ly(), tiny4)
  expect_true(inherits(p, "plotly"))
})

test_that("add_dendro() result is pipeable into layout()", {
  p <- plotly::plot_ly() |>
    add_dendro(tiny4) |>
    plotly::layout(title = "added layer")
  expect_s3_class(p, "plotly")
})

# --- Layer functions can be chained --------------------------------------

test_that("layer functions can be chained", {
  d <- dendro_data(tiny4)
  p <- plotly::plot_ly() |>
    add_dendro_segments(d) |>
    add_dendro_labels(d) |>
    add_dendro_nodes(d)
  expect_s3_class(p, "plotly")
})

test_that("add_dendro() forwards hang into rendered segment geometry", {
  p_default <- plotly::plot_ly() |> add_dendro(tiny4)
  p_hang <- plotly::plot_ly() |> add_dendro(tiny4, hang = 0.1)

  built_default <- plotly::plotly_build(p_default)
  built_hang <- plotly::plotly_build(p_hang)

  expect_false(identical(built_default$x$data[[1]]$y, built_hang$x$data[[1]]$y))
  expect_identical(built_hang$x$layout$xaxis$ticktext, c("B", "C", "A", "D"))
})
