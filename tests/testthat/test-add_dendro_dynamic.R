.dyn_fixture <- function(n = 100, seed = 5) {
  set.seed(seed)
  hc <- stats::hclust(stats::dist(matrix(stats::rnorm(n * 4), ncol = 4)))
  list(hc = hc, d = dendro_data(hc, hang = 0.03))
}

.dyn_cfg <- function(p) p$jsHooks$render[[1L]]$data

test_that("add_dendro_dynamic() attaches the runtime dependency and render hook", {
  f <- .dyn_fixture()
  p <- add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20)

  names <- vapply(p$dependencies, function(d) d$name, character(1))
  expect_true("plotly-dendro" %in% names)

  dep <- p$dependencies[[which(names == "plotly-dendro")[1L]]]
  expect_setequal(
    dep$script,
    c("dendro-peel.js", "dendro-geometry.js", "plotly-dendro.js")
  )
  expect_true(all(file.exists(file.path(dep$src$file, dep$script))))

  expect_length(p$jsHooks$render, 1L)
})

test_that("the runtime owns one trace per palette entry plus one for branches", {
  f <- .dyn_fixture()
  colors <- rep(c("#e41a1c", "#377eb8", "#4daf4a"), length.out = nrow(f$d$labels))

  p <- add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20, leaf_color = colors)
  traces <- plotly::plotly_build(p)$x$data

  expect_length(traces, 4L)
  metas <- vapply(traces, function(t) t$meta %||% "", character(1))
  expect_identical(
    metas,
    c("plotly-dendro:clade:0", "plotly-dendro:clade:1",
      "plotly-dendro:clade:2", "plotly-dendro:segments")
  )

  # Placeholders carry no drawable geometry; the browser fills them on first
  # render. plotly's NSE resolves the empty vectors to their own symbol names,
  # so what actually ships is the string "x"/"y" -- assert that rather than a
  # zero-length array, and assert the axis type that keeps it harmless.
  for (t in traces) {
    expect_false(is.numeric(t$x))
    expect_false(is.numeric(t$y))
  }
  expect_identical(plotly::plotly_build(p)$x$layout$xaxis$type, "linear")
})

test_that("traces are tagged rather than positioned, so caller traces do not shift them", {
  f <- .dyn_fixture()

  p <- plotly::plot_ly() |>
    plotly::add_trace(x = 1:3, y = 1:3, type = "scatter", mode = "markers") |>
    add_dendro_dynamic(f$d, max_leaves = 20)

  metas <- vapply(plotly::plotly_build(p)$x$data, function(t) t$meta %||% "", character(1))
  expect_identical(sum(grepl("^plotly-dendro:", metas)), 2L)
  expect_true("plotly-dendro:segments" %in% metas)
})

test_that("the payload ships the node table, not the segment table", {
  f <- .dyn_fixture()
  cfg <- .dyn_cfg(add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20))

  n <- length(f$hc$order)
  expect_equal(cfg$n, n)
  expect_length(cfg$nodes$members, n - 1L)
  expect_equal(cfg$nodes$height, as.numeric(f$hc$height))
  expect_equal(cfg$nodes$span_hi - cfg$nodes$span_lo + 1L, cfg$nodes$members)

  # The whole point of shipping nodes: far smaller than the segments it replaces.
  expect_lt(length(cfg$nodes$members) * 7, nrow(f$d$segments) * 4)
})

test_that("leaves are keyed by hclust leaf index and labels by display position", {
  f <- .dyn_fixture()
  cfg <- .dyn_cfg(add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20))

  n <- length(f$hc$order)
  leaf_row <- integer(n)
  leaf_row[f$hc$order] <- seq_len(n)

  # leaves[j] describes the leaf whose hclust index is j.
  expect_equal(cfg$leaves$x, as.numeric(leaf_row))
  expect_equal(cfg$leaves$y, f$d$labels$y[leaf_row])

  # labels[p] describes the leaf at display position p.
  expect_identical(cfg$labels, as.character(f$d$labels$label))

  # The two orderings must genuinely differ, or this test proves nothing.
  expect_false(identical(leaf_row, seq_len(n)))
})

test_that("the wedge base sits on the leaf floor rather than at a fixed zero", {
  f <- .dyn_fixture()
  cfg <- .dyn_cfg(add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20))
  expect_equal(cfg$base, min(f$d$labels$y))

  # Without hang the floor genuinely is zero, and the same expression holds.
  flat <- dendro_data(f$hc)
  expect_equal(.dyn_cfg(add_dendro_dynamic(plotly::plot_ly(), flat, max_leaves = 20))$base, 0)

  # On a tree whose merges all sit well above the hang offset, no leaf is
  # clamped to zero, so the floor lifts off it. (Where the lowest merges fall
  # below the offset, hclust clamps those leaves to zero and the floor stays
  # there -- which is why this needs its own fixture rather than an assertion
  # on the one above.)
  hc <- f$hc
  hc$height <- hc$height / max(hc$height) * 0.2 + 0.8
  lifted <- dendro_data(hc, hang = 0.03)
  expect_gt(
    .dyn_cfg(add_dendro_dynamic(plotly::plot_ly(), lifted, max_leaves = 20))$base,
    0
  )
})

test_that("leaf_color is resolved to a palette plus zero-based indices", {
  f <- .dyn_fixture()
  colors <- rep(c("#e41a1c", "#377eb8"), length.out = nrow(f$d$labels))
  cfg <- .dyn_cfg(add_dendro_dynamic(
    plotly::plot_ly(), f$d,
    max_leaves = 20, leaf_color = colors
  ))

  expect_identical(cfg$palette, c("#e41a1c", "#377eb8"))
  expect_equal(cfg$leaf_color, match(colors, cfg$palette) - 1L)
  expect_equal(min(cfg$leaf_color), 0L)
  expect_equal(cfg$n_clade_traces, 2L)
})

test_that("the leaf axis opens on the full tree", {
  f <- .dyn_fixture()
  n <- nrow(f$d$labels)

  lay <- plotly::plotly_build(
    add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20)
  )$x$layout
  expect_equal(lay$xaxis$range, c(0.5, n + 0.5))

  # "right" mirrors the leaf axis, so the pinned range must mirror too.
  lay_r <- plotly::plotly_build(
    add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20, orientation = "right")
  )$x$layout
  expect_equal(lay_r$yaxis$range, c(-(n + 0.5), -0.5))
})

test_that("add_dendro_dynamic() rejects input it cannot serve", {
  f <- .dyn_fixture()

  expect_error(
    add_dendro_dynamic(plotly::plot_ly(), list(labels = NULL)),
    "hclust"
  )
  expect_error(
    add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = Inf),
    "must be finite"
  )
  expect_error(
    add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20, leaf_color = c("red", "blue")),
    "one entry per leaf"
  )
  # A custom priority cannot be guaranteed monotone, so the runtime rejects it.
  expect_error(
    add_dendro_dynamic(plotly::plot_ly(), f$d, max_leaves = 20, priority = function(m, h) m),
    class = "error"
  )
})
