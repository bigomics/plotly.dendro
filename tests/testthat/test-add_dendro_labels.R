# add_dendro_labels() — layer function adding leaf label text traces
# Reference: plotly R class contract c("plotly", "htmlwidget").
# Gate: returns a valid plotly object when called on dendro_data() output.

skip_if_not_installed("plotly")

test_that("add_dendro_labels() returns a plotly", {
  d <- dendro_data(tiny4)
  p <- add_dendro_labels(plotly::plot_ly(), d)
  expect_s3_class(p, "plotly")
})
