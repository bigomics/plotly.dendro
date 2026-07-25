.hr_fixture <- function(hang = 0.03) {
  set.seed(11)
  hc <- stats::hclust(stats::dist(matrix(stats::rnorm(200), ncol = 4)))
  list(hc = hc, d = dendro_data(hc, hang = hang))
}

.layout_of <- function(p) plotly::plotly_build(p)$x$layout

test_that("height_range='data' leaves the height axis unset so plotly autoranges", {
  f <- .hr_fixture()
  lay <- .layout_of(add_dendro_layout(plotly::plot_ly(), f$d))

  expect_null(lay$yaxis$range)
  expect_null(lay$yaxis$autorange)
})

test_that("height_range='data' preserves the pre-existing layout exactly", {
  f <- .hr_fixture()
  # The default must be a no-op relative to the behaviour that shipped before
  # height_range existed, so downstream snapshots do not move.
  explicit <- .layout_of(add_dendro_layout(plotly::plot_ly(), f$d, height_range = "data"))
  bare <- .layout_of(add_dendro_layout(plotly::plot_ly(), f$d))
  expect_identical(explicit, bare)
})

test_that("height_range='full' anchors the axis at zero", {
  f <- .hr_fixture()
  lay <- .layout_of(add_dendro_layout(plotly::plot_ly(), f$d, height_range = "full"))

  expect_equal(lay$yaxis$range, c(0, max(f$hc$height)))
  expect_false(lay$yaxis$autorange)
})

test_that("a numeric height_range clips the axis and is order-insensitive", {
  f <- .hr_fixture()

  lay <- .layout_of(add_dendro_layout(plotly::plot_ly(), f$d, height_range = c(1, 4)))
  expect_equal(lay$yaxis$range, c(1, 4))

  reversed <- .layout_of(add_dendro_layout(plotly::plot_ly(), f$d, height_range = c(4, 1)))
  expect_equal(reversed$yaxis$range, c(1, 4))
})

test_that("the height range lands on the correct axis for each orientation", {
  f <- .hr_fixture()
  rng <- c(1, 4)

  bottom <- .layout_of(add_dendro_layout(plotly::plot_ly(), f$d, height_range = rng))
  expect_equal(bottom$yaxis$range, rng)
  expect_null(bottom$xaxis$range)

  # "left"/"right" swap the axes, so height becomes the x axis.
  for (orientation in c("left", "right")) {
    lay <- .layout_of(add_dendro_layout(
      plotly::plot_ly(), f$d,
      orientation = orientation, height_range = rng
    ))
    expect_equal(lay$xaxis$range, rng, info = orientation)
    expect_null(lay$yaxis$range)
  }
})

test_that("'top' negates the height range to match the flipped geometry", {
  f <- .hr_fixture()
  lay <- .layout_of(add_dendro_layout(
    plotly::plot_ly(), f$d,
    orientation = "top", height_range = c(1, 4)
  ))
  expect_equal(lay$yaxis$range, c(-4, -1))
})

test_that("height_range rejects malformed input", {
  f <- .hr_fixture()
  expect_error(add_dendro_layout(plotly::plot_ly(), f$d, height_range = 1), "two finite numbers")
  expect_error(
    add_dendro_layout(plotly::plot_ly(), f$d, height_range = c(1, NA)),
    "two finite numbers"
  )
  expect_error(
    add_dendro_layout(plotly::plot_ly(), f$d, height_range = c(0, Inf)),
    "two finite numbers"
  )
})

test_that("hang lifts leaves off the floor, which is what makes 'data' useful", {
  set.seed(11)
  hc <- stats::hclust(stats::dist(matrix(stats::rnorm(200), ncol = 4)))

  flat <- dendro_data(hc)
  hung <- dendro_data(hc, hang = 0.03)

  expect_equal(min(flat$labels$y), 0)
  expect_gt(min(hung$labels$y), 0)
})

test_that("plot_dendro() threads max_leaves and height_range through", {
  set.seed(11)
  hc <- stats::hclust(stats::dist(matrix(stats::rnorm(200), ncol = 4)))

  p <- plot_dendro(hc, hang = 0.03, max_leaves = 12, height_range = "full")
  d <- attr(p, "dendro_data")

  expect_equal(nrow(d$clades), 12)
  expect_equal(.layout_of(p)$yaxis$range, c(0, max(hc$height)))

  # Unbudgeted plots must not gain a clades component at all.
  plain <- plot_dendro(hc, hang = 0.03)
  expect_null(attr(plain, "dendro_data")$clades)
})
