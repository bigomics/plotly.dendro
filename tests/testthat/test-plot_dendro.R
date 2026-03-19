# plot_dendro() — entry-point function creating a standalone plotly dendrogram
# Reference: plotly R class contract c("plotly", "htmlwidget").
# Gate: returns valid pipeable plotly object; accepts all input types and orientations.

skip_if_not_installed("plotly")

test_that("plot_dendro() returns a plotly object", {
  p <- plot_dendro(tiny4)
  expect_s3_class(p, "plotly")
})

test_that("plot_dendro() returns an htmlwidget", {
  p <- plot_dendro(tiny4)
  expect_s3_class(p, "htmlwidget")
})

test_that("plot_dendro() is pipeable into layout()", {
  p <- plot_dendro(tiny4) |>
    plotly::layout(title = "test dendrogram")
  expect_s3_class(p, "plotly")
})

test_that("plot_dendro() is pipeable into config()", {
  p <- plot_dendro(tiny4) |>
    plotly::config(displayModeBar = FALSE)
  expect_s3_class(p, "plotly")
})

test_that("plot_dendro() accepts a matrix input", {
  expect_s3_class(plot_dendro(tiny4), "plotly")
})

test_that("plot_dendro() accepts a dist input", {
  expect_s3_class(plot_dendro(dist(tiny4)), "plotly")
})

test_that("plot_dendro() accepts an hclust input", {
  hc <- hclust_complete(dist(tiny4))
  expect_s3_class(plot_dendro(hc), "plotly")
})

test_that("all four orientations produce a valid plotly object", {
  for (orient in c("top", "bottom", "left", "right")) {
    p <- plot_dendro(tiny4, orientation = orient)
    expect_s3_class(p, "plotly", label = paste("orientation:", orient))
  }
})

# --- subplot() compatibility ---------------------------------------------

test_that("plot_dendro() is compatible with subplot()", {
  skip_if_not_installed("plotly")
  p1 <- plot_dendro(tiny4, orientation = "top")
  p2 <- plotly::plot_ly(x = seq_len(nrow(tiny4)), y = seq_len(nrow(tiny4)),
                        type = "scatter", mode = "markers")
  sp <- plotly::subplot(p1, p2, nrows = 2, shareX = TRUE)
  expect_s3_class(sp, "plotly")
})

test_that("plot_dendro() forwards hang into rendered segment geometry", {
  built_default <- plotly::plotly_build(plot_dendro(tiny4))
  built_hang <- plotly::plotly_build(plot_dendro(tiny4, hang = 0.1))

  expect_false(identical(built_default$x$data[[1]]$y, built_hang$x$data[[1]]$y))
  expect_identical(built_hang$x$layout$xaxis$ticktext, c("B", "C", "A", "D"))
})
