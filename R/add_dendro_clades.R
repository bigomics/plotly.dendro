#' Add Collapsed-Clade Glyph Layer
#'
#' Draws the collapsed frontier produced by [dendro_cut()]. Each clade becomes
#' one glyph standing in for the subtree beneath it.
#'
#' The default `"wedge"` shape is an isosceles triangle with its apex at the
#' clade's merge height and its base spanning the clade's true leaf range.
#' Because the base width *is* the leaf span, glyph area encodes leaf count
#' with no additional scaling, and the glyph stays aligned with anything else
#' drawn against the leaf axis (an annotation strip, say) for free.
#'
#' The apex sits at the midpoint of the leaf span rather than at the clade's
#' own node position. The latter is geometrically truthful — it is where the
#' clade attaches to its parent — but on an unbalanced clade it lands near one
#' end of the span, rendering a lopsided right triangle that reads as a drawing
#' error rather than as data.
#'
#' @param p A plotly object.
#' @param dendro A [dendro_cut()] result.
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#' @param shape Glyph shape. `"wedge"` (default) draws size-encoding triangles;
#'   `"stub"` draws a plain line from the base to the merge height, the
#'   convention `scipy` uses; `"none"` draws nothing, for callers that want to
#'   render `dendro$clades` themselves.
#' @param fill Glyph fill. A single color, a vector of one color per clade (in
#'   `dendro$clades` row order), or a function of the clades data frame
#'   returning such a vector. Ignored when `shape` is not `"wedge"`.
#' @param base Leaf-axis baseline for the glyphs. Defaults to the minimum leaf
#'   position in the underlying geometry, which respects `hang`.
#' @param line A named list of plotly line properties for the glyph outline.
#' @param ... Passed to `plotly::add_trace()`.
#' @param inherit Plotly inheritance flag.
#'
#' @return A plotly object with the clade layer added.
#'
#' @details
#' All glyphs sharing a fill color are emitted as a *single* trace, their
#' polygons concatenated with `NA` separators. Plotly's `fill = "toself"`
#' closes each gap-delimited run independently, so a few hundred clades cost
#' one trace per distinct color rather than one trace per clade. Trace count is
#' the dominant scaling factor in plotly.js, so this matters more than the
#' vertex count does.
#'
#' @seealso [dendro_cut()]
#'
#' @examples
#' hc <- stats::hclust(stats::dist(matrix(rnorm(400), ncol = 4)))
#' d <- dendro_cut(dendro_data(hc, hang = 0.03), max_leaves = 20)
#' plotly::plot_ly() |>
#'   add_dendro_clades(d) |>
#'   add_dendro_segments(d)
#'
#' @export
add_dendro_clades <- function(
    p,
    dendro,
    orientation = "bottom",
    shape = c("wedge", "stub", "none"),
    fill = NULL,
    base = NULL,
    line = NULL,
    ...,
    inherit = FALSE
) {
  .validate_orientation(orientation)
  shape <- match.arg(shape)

  clades <- dendro$clades
  if (is.null(clades)) {
    stop(
      "add_dendro_clades() requires a dendro_cut() result; ",
      "`dendro$clades` is missing.",
      call. = FALSE
    )
  }
  if (shape == "none" || nrow(clades) == 0L) {
    return(p)
  }

  # The glyph floor is the lowest leaf, not zero: with `hang` the leaves sit
  # well above zero, and basing wedges at zero would stretch every one of them
  # through empty space below the tree.
  base <- base %||% min(dendro$labels$y)
  line <- line %||% list(color = default_colorscale()[1], width = 0.4)

  if (shape == "stub") {
    return(.add_clade_stubs(p, clades, orientation, base, line, inherit, list(...)))
  }

  fills <- .resolve_clade_fill(fill, clades)
  .add_clade_wedges(p, clades, orientation, base, line, fills, inherit, list(...))
}

# Fill may be absent, a single color, one color per clade, or a function of the
# clades table. The package never derives fills from domain data — a caller
# that wants, say, dominant-module coloring computes it and passes it in.
.resolve_clade_fill <- function(fill, clades) {
  if (is.function(fill)) {
    fill <- fill(clades)
  }
  if (is.null(fill)) {
    fill <- default_colorscale()[1]
  }
  if (!is.character(fill)) {
    stop("fill must be a character vector or a function returning one.", call. = FALSE)
  }
  if (length(fill) == 1L) {
    return(rep.int(fill, nrow(clades)))
  }
  if (length(fill) != nrow(clades)) {
    stop(
      sprintf(
        "fill must have 1 or %d entries, one per clade; got %d.",
        nrow(clades), length(fill)
      ),
      call. = FALSE
    )
  }
  fill
}

# Concatenates equal-length coordinate runs into one vector, separating
# adjacent runs with NA. No trailing separator: plotly drops it on
# serialization, and relying on that would make the payload length ambiguous.
.na_join <- function(...) {
  cols <- rbind(..., NA_real_)
  as.numeric(cols)[seq_len(length(cols) - 1L)]
}

# Triangle vertices per clade: left base, right base, apex, back to left base.
.clade_wedge_xy <- function(clades, base) {
  lo <- clades$span_lo - 0.5
  hi <- clades$span_hi + 0.5
  list(
    x = .na_join(lo, hi, (lo + hi) / 2, lo),
    y = .na_join(base, base, clades$height, base)
  )
}

.add_clade_wedges <- function(p, clades, orientation, base, line, fills, inherit, dots) {
  for (color in unique(fills)) {
    part <- clades[fills == color, , drop = FALSE]
    xy <- .clade_wedge_xy(part, base)
    poly <- .orient_xy(
      data.frame(x = xy$x, y = xy$y, stringsAsFactors = FALSE),
      orientation
    )
    p <- do.call(
      plotly::add_trace,
      c(list(
        p = p,
        x = poly$x,
        y = poly$y,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = color,
        line = line,
        connectgaps = FALSE,
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = inherit
      ), dots)
    )
  }
  p
}

.add_clade_stubs <- function(p, clades, orientation, base, line, inherit, dots) {
  stub <- .orient_xy(
    data.frame(
      x = .na_join(clades$x, clades$x),
      y = .na_join(rep_len(base, nrow(clades)), clades$height),
      stringsAsFactors = FALSE
    ),
    orientation
  )
  do.call(
    plotly::add_trace,
    c(list(
      p = p,
      x = stub$x,
      y = stub$y,
      type = "scatter",
      mode = "lines",
      line = line,
      connectgaps = FALSE,
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = inherit
    ), dots)
  )
}
