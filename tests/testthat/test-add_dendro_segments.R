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
