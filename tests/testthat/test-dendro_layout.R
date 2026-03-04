# Phase 6 — Layout construction: dendro_layout()
# Reference: Python plotly tickvals/ticktext contract + ggdendro leaf order.
# Gate: tickvals count == N, ticktext order matches ggdendro, tickmode == "array".

skip_if_not_installed("ggdendro")

make_dendro_list <- function(data, method = "complete") {
  hc <- stats::hclust(stats::dist(data), method = method)
  list(
    segments = extract_segments(hc),
    labels   = extract_labels(hc),
    nodes    = extract_nodes(hc)
  )
}

# --- Return type ---------------------------------------------------------

test_that("dendro_layout() returns a list", {
  d  <- make_dendro_list(tiny4)
  ly <- dendro_layout(d, "bottom")
  expect_type(ly, "list")
})

test_that("dendro_layout() contains xaxis and yaxis keys", {
  d  <- make_dendro_list(tiny4)
  ly <- dendro_layout(d, "bottom")
  expect_true("xaxis" %in% names(ly))
  expect_true("yaxis" %in% names(ly))
})

# --- tickvals ------------------------------------------------------------

test_that("tickvals count equals N leaves [tiny4]", {
  d  <- make_dendro_list(tiny4)
  ly <- dendro_layout(d, "bottom")
  expect_length(ly$xaxis$tickvals, nrow(tiny4))
})

test_that("tickvals count equals N leaves [medium]", {
  d  <- make_dendro_list(medium)
  ly <- dendro_layout(d, "bottom")
  expect_length(ly$xaxis$tickvals, nrow(medium))
})

test_that("tickvals are all finite numerics", {
  d  <- make_dendro_list(medium)
  ly <- dendro_layout(d, "bottom")
  expect_true(all(is.finite(ly$xaxis$tickvals)))
})

# --- ticktext matches ggdendro leaf order --------------------------------

test_that("ticktext order matches ggdendro left-to-right order [tiny4]", {
  hc        <- hclust_complete(dist(tiny4))
  ref_lbs   <- ggdendro::label(ggdendro::dendro_data(hc, type = "rectangle"))
  ref_order <- ref_lbs$label[order(ref_lbs$x)]

  d  <- list(segments = extract_segments(hc),
             labels   = extract_labels(hc),
             nodes    = extract_nodes(hc))
  ly <- dendro_layout(d, "bottom")
  expect_equal(ly$xaxis$ticktext, ref_order)
})

test_that("ticktext order matches ggdendro left-to-right order [medium]", {
  hc        <- hclust_complete(dist(medium))
  ref_lbs   <- ggdendro::label(ggdendro::dendro_data(hc, type = "rectangle"))
  ref_order <- ref_lbs$label[order(ref_lbs$x)]

  d  <- list(segments = extract_segments(hc),
             labels   = extract_labels(hc),
             nodes    = extract_nodes(hc))
  ly <- dendro_layout(d, "bottom")
  expect_equal(ly$xaxis$ticktext, ref_order)
})

# --- tickmode ------------------------------------------------------------

test_that("tickmode is 'array'", {
  d  <- make_dendro_list(tiny4)
  ly <- dendro_layout(d, "bottom")
  expect_equal(ly$xaxis$tickmode, "array")
})

# --- Orientation: horizontal puts ticks on yaxis -------------------------

test_that("orientation 'left' puts tickvals on yaxis, not xaxis", {
  d   <- make_dendro_list(tiny4)
  od  <- orient_data(d, "left")
  ly  <- dendro_layout(od, "left")
  expect_false(is.null(ly$yaxis$tickvals))
  expect_null(ly$xaxis$tickvals)
})

test_that("orientation 'right' puts tickvals on yaxis, not xaxis", {
  d   <- make_dendro_list(tiny4)
  od  <- orient_data(d, "right")
  ly  <- dendro_layout(od, "right")
  expect_false(is.null(ly$yaxis$tickvals))
  expect_null(ly$xaxis$tickvals)
})

test_that("orientation 'top' keeps tickvals on xaxis", {
  d   <- make_dendro_list(tiny4)
  od  <- orient_data(d, "top")
  ly  <- dendro_layout(od, "top")
  expect_false(is.null(ly$xaxis$tickvals))
})

# --- Axis display settings -----------------------------------------------

test_that("dendro_layout() turns off grid and zeroline on leaf axis", {
  d  <- make_dendro_list(tiny4)
  ly <- dendro_layout(d, "bottom")
  expect_false(isTRUE(ly$xaxis$showgrid))
  expect_false(isTRUE(ly$xaxis$zeroline))
})
