.expected_edge_colors <- function(hc, leaf_colors, default_color) {
  uniform <- character(nrow(hc$merge))
  edge_colors <- character(4L * nrow(hc$merge))
  leaf_by_original <- character(length(hc$order))
  leaf_by_original[hc$order] <- leaf_colors
  cursor <- 1L

  child_color <- function(child) {
    if (child < 0L) leaf_by_original[-child] else uniform[child]
  }
  for (row in seq_len(nrow(hc$merge))) {
    left <- child_color(hc$merge[row, 1L])
    right <- child_color(hc$merge[row, 2L])
    uniform[row] <- if (!is.na(left) && identical(left, right)) left else NA_character_
  }

  stack <- list(list(node = nrow(hc$merge), state = 0L))
  while (length(stack)) {
    frame <- stack[[length(stack)]]
    if (frame$state < 2L) {
      column <- frame$state + 1L
      stack[[length(stack)]]$state <- column
      child <- hc$merge[frame$node, column]
      color <- child_color(child)
      if (is.na(color)) color <- default_color
      edge_colors[cursor:(cursor + 1L)] <- color
      cursor <- cursor + 2L
      if (child > 0L) {
        stack[[length(stack) + 1L]] <- list(node = child, state = 0L)
      }
    } else {
      stack <- stack[-length(stack)]
    }
  }
  edge_colors
}

test_that("native branch coloring matches bottom-up subtree uniformity", {
  hc <- .comb_hclust(24L)
  d <- dendro_data(hc, nodes = FALSE)
  palette <- c("red", "blue", "green")

  for (threshold in c(0, 2, 8, max(hc$height) + 1)) {
    leaf_colors <- .leaf_cluster_colors(hc, threshold, palette)
    expected <- .expected_edge_colors(hc, leaf_colors, palette[1])
    observed <- color_branches(
      d$segments,
      hc,
      color_threshold = threshold,
      colorscale = palette
    )
    expect_identical(observed$color, expected)
  }
})

test_that("native branch coloring handles palette cycling and duplicates", {
  hc <- .balanced_hclust()
  d <- dendro_data(hc, nodes = FALSE)

  cycled <- color_branches(
    d$segments,
    hc,
    color_threshold = 0,
    colorscale = c("red", "blue")
  )
  expect_true(all(cycled$color %in% c("red", "blue")))

  duplicated <- color_branches(
    d$segments,
    hc,
    color_threshold = 0,
    colorscale = c("red", "red", "blue")
  )
  expect_true(all(duplicated$color %in% c("red", "blue")))
})

test_that("inactive coloring uses the uniform short circuit", {
  hc <- .balanced_hclust()
  d <- dendro_data(hc, nodes = FALSE)
  colored <- color_branches(
    d$segments,
    hc,
    color_threshold = NULL,
    colorscale = c("purple", "orange")
  )
  expect_identical(
    colored$color,
    rep("purple", nrow(colored))
  )
})

test_that("noncanonical segment tables use the compatibility fallback", {
  hc <- .balanced_hclust()
  d <- dendro_data(hc, nodes = FALSE)
  reordered <- d$segments[rev(seq_len(nrow(d$segments))), , drop = FALSE]

  direct <- cpp_hclust_branch_ids(
    hc$merge,
    hc$height,
    hc$order,
    reordered$x,
    reordered$y,
    reordered$xend,
    reordered$yend,
    seq_along(hc$order)
  )
  expect_null(direct)
  canonical <- color_branches(
    d$segments,
    hc,
    color_threshold = 0,
    colorscale = default_colorscale()
  )
  fallback <- color_branches(
    reordered,
    hc,
    color_threshold = 0,
    colorscale = default_colorscale()
  )
  expect_identical(fallback$color, rev(canonical$color))
})

test_that("native branch input validation is safe", {
  hc <- .balanced_hclust()
  d <- dendro_data(hc, nodes = FALSE)

  expect_error(
    cpp_hclust_branch_ids(
      hc$merge,
      hc$height,
      hc$order,
      d$segments$x,
      d$segments$y,
      d$segments$xend,
      d$segments$yend,
      integer()
    ),
    "one entry per displayed leaf"
  )
  expect_error(
    cpp_hclust_branch_ids(
      hc$merge,
      hc$height,
      hc$order,
      d$segments$x,
      d$segments$y,
      d$segments$xend,
      d$segments$yend,
      rep(NA_integer_, length(hc$order))
    ),
    "positive non-missing"
  )
  expect_error(
    color_branches(
      d$segments,
      hc,
      color_threshold = 0,
      colorscale = character()
    ),
    "non-empty character vector"
  )
})
