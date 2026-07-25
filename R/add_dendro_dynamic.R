#' Add A Zoom-Driven Dendrogram Layer
#'
#' Draws a dendrogram whose level of detail follows the viewport: collapsed
#' clades open into their sub-branches as the user zooms in, and close again as
#' they zoom out, holding the number of drawn clades near `max_leaves` at every
#' zoom level. Unlike a statically cut plot, zooming *resolves* detail rather
#' than merely magnifying it.
#'
#' Runs entirely in the browser. No Shiny session and no server round-trip are
#' required, so the result works in a self-contained HTML file.
#'
#' @param p A plotly object.
#' @param dendro A [dendro_data()] result carrying an `"hclust"` attribute.
#' @param max_leaves Maximum number of clades to draw at any one time.
#' @param budget_mode `"fixed"` always targets `max_leaves`. `"pixels"` (the
#'   default) additionally caps the budget at `plot_px / min_leaf_px`, so a
#'   narrow panel never draws more clades than it has room to distinguish.
#' @param min_leaf_px Pixels each clade needs before it is worth drawing. Only
#'   consulted when `budget_mode = "pixels"`.
#' @param priority How to spend the budget: `"members"`, `"height"`, or
#'   `"hybrid"`. See [dendro_cut()]. Custom priority functions are not
#'   supported here — see Details.
#' @param min_clade Stop subdividing clades below this leaf count.
#' @param shape Clade glyph: `"wedge"` or `"stub"`.
#' @param leaf_color Optional per-leaf colour, in leaf display order, used to
#'   tint each clade by the colour that dominates it. This is what lets a
#'   collapsed clade say *what* is inside it rather than only how much.
#' @param fill Fallback clade fill when `leaf_color` is absent.
#' @param line A named list of plotly line properties for branches and glyph
#'   outlines.
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#' @param show_labels Draw leaf tick labels when the zoom level leaves room.
#' @param label_budget Maximum leaf labels to draw at once.
#' @param uirevision Value held constant across updates so plotly preserves the
#'   user's zoom.
#'
#' @return The plotly object with placeholder traces and the runtime attached.
#'
#' @details
#' The browser re-cuts the tree on `plotly_relayout`, restricted to the visible
#' leaf window, and pushes the result with `Plotly.react`. Because
#' `Plotly.react` emits `plotly_react` and never `plotly_relayout`, the handler
#' cannot re-trigger itself: there is no relayout feedback loop to guard
#' against. Updates are additionally debounced and skipped when the recomputed
#' window and budget would reproduce the cut already on screen.
#'
#' Only monotone priorities are permitted. The runtime selects the visible cut
#' by thresholding the score, which yields a valid top-down-closed cut only
#' when a parent never scores below its children. All three built-in priorities
#' satisfy this; an arbitrary user function need not, so custom scores are
#' available in [dendro_cut()] (static) but not here.
#'
#' @seealso [dendro_cut()] for a static cut, [add_dendro_clades()] for the
#'   glyph layer it shares.
#'
#' @examples
#' \dontrun{
#' hc <- stats::hclust(stats::dist(matrix(rnorm(4000), ncol = 4)))
#' d <- dendro_data(hc, hang = 0.03)
#' plotly::plot_ly() |>
#'   add_dendro_dynamic(d, max_leaves = 300) |>
#'   add_dendro_layout(d)
#' }
#'
#' @export
add_dendro_dynamic <- function(
    p,
    dendro,
    max_leaves = 300,
    budget_mode = c("pixels", "fixed"),
    min_leaf_px = 3,
    priority = c("members", "height", "hybrid"),
    min_clade = 1L,
    shape = c("wedge", "stub"),
    leaf_color = NULL,
    fill = NULL,
    line = NULL,
    orientation = "bottom",
    show_labels = TRUE,
    label_budget = 50,
    uirevision = "plotly-dendro"
) {
  .validate_orientation(orientation)
  budget_mode <- match.arg(budget_mode)
  priority <- match.arg(priority)
  shape <- match.arg(shape)

  hc <- attr(dendro, "hclust")
  if (is.null(hc)) {
    stop(
      "add_dendro_dynamic() requires a dendro_data() result with an 'hclust' attribute.",
      call. = FALSE
    )
  }

  max_leaves <- .validate_max_leaves(max_leaves)
  if (!is.finite(max_leaves)) {
    stop(
      "max_leaves must be finite for a dynamic dendrogram; ",
      "an unbounded budget defeats the purpose.",
      call. = FALSE
    )
  }
  min_clade <- .validate_min_clade(min_clade)

  inputs <- .normalize_hclust_inputs(hc)
  tbl <- node_table(inputs$merge, inputs$height, inputs$order)

  palette <- .dynamic_palette(leaf_color, fill, dendro)
  line <- line %||% list(color = default_colorscale()[1], width = 0.9)

  # One trace per palette slot plus one for the branches. The count is fixed
  # for the life of the widget: the runtime refills these in place, and an
  # unequal trace count would force plotly into a full replot on every update.
  #
  # Each trace is tagged via plotly's `meta`, and the runtime locates them by
  # tag rather than by index. Index arithmetic is not reliable here — plot_ly()
  # seeds an empty attrs entry that never becomes a trace, and a caller may add
  # further traces before or after this call.
  for (i in seq_along(palette$colors)) {
    p <- .add_placeholder_trace(
      p, orientation,
      fill = "toself", fillcolor = palette$colors[i],
      line = list(color = line$color, width = 0.4),
      meta = paste0(.dendro_tag, "clade:", i - 1L)
    )
  }
  p <- .add_placeholder_trace(
    p, orientation,
    fill = "none", fillcolor = NULL, line = line,
    meta = paste0(.dendro_tag, "segments")
  )

  # The runtime derives its first cut from the leaf axis range, but the traces
  # it will fill are still empty at that point, so plotly would autorange to a
  # meaningless default and open on a handful of leaves. Pin the full leaf
  # extent up front; the user's own zooming takes over from there.
  p <- .apply_leaf_range(p, nrow(dendro$labels), orientation)

  cfg <- .build_dynamic_config(
    dendro = dendro,
    tbl = tbl,
    palette = palette,
    max_leaves = max_leaves,
    budget_mode = budget_mode,
    min_leaf_px = min_leaf_px,
    priority = priority,
    min_clade = min_clade,
    shape = shape,
    orientation = orientation,
    show_labels = show_labels,
    label_budget = label_budget,
    uirevision = uirevision
  )

  .attach_dendro_runtime(p, cfg)
}

# Tag prefix written into each runtime-owned trace's `meta`, so the browser can
# find its traces without relying on their position.
.dendro_tag <- "plotly-dendro:"

# Pins the leaf axis to the full tree extent, mirroring the flip .orient_xy()
# applies to the data: "left" moves the leaf axis to y, and "right" moves and
# negates it.
.apply_leaf_range <- function(p, n, orientation) {
  extent <- c(0.5, n + 0.5)
  # type = "linear" is not cosmetic: the placeholder traces serialize their
  # coordinates as strings (see .add_placeholder_trace()), and without an
  # explicit type plotly would infer a category axis from them.
  spec <- list(range = extent, type = "linear")
  flipped <- list(range = c(-extent[2L], -extent[1L]), type = "linear")

  axis <- switch(
    orientation,
    bottom = list(xaxis = spec),
    top = list(xaxis = spec),
    left = list(yaxis = spec),
    right = list(yaxis = flipped)
  )
  do.call(plotly::layout, c(list(p), axis))
}

# An empty line trace the runtime fills in on every update. Created with the
# final styling so a redraw only ever has to replace coordinates.
.add_placeholder_trace <- function(p, orientation, fill, fillcolor, line, meta) {
  args <- list(
    p = p,
    # plotly's non-standard evaluation resolves an empty vector to its own
    # symbol name, so these serialize as the literal string "x"/"y" rather than
    # as empty arrays. Harmless -- the runtime replaces both on its first pass,
    # before the placeholder is ever drawn -- and the alternatives are worse:
    # passing NA instead makes plotly drop `meta`, which is how the runtime
    # finds these traces at all. `.apply_leaf_range()` pins the axis type so
    # the string cannot make plotly infer a category axis in the meantime.
    x = numeric(0),
    y = numeric(0),
    type = "scatter",
    mode = "lines",
    line = line,
    meta = meta,
    connectgaps = FALSE,
    hoverinfo = "skip",
    showlegend = FALSE,
    inherit = FALSE
  )
  if (!identical(fill, "none")) {
    args$fill <- fill
    args$fillcolor <- fillcolor
  }
  do.call(plotly::add_trace, args)
}

# Resolves clade colouring into a palette plus a per-leaf palette index. The
# package never derives colours from domain data: a caller that wants, say,
# module colouring passes the per-leaf vector in.
.dynamic_palette <- function(leaf_color, fill, dendro) {
  if (is.null(leaf_color)) {
    return(list(
      colors = fill %||% default_colorscale()[1],
      index = NULL
    ))
  }

  n <- nrow(dendro$labels)
  if (length(leaf_color) != n) {
    stop(
      sprintf("leaf_color must have one entry per leaf (%d); got %d.",
              n, length(leaf_color)),
      call. = FALSE
    )
  }

  colors <- unique(as.character(leaf_color))
  list(
    colors = colors,
    index = match(as.character(leaf_color), colors) - 1L  # 0-based for JS
  )
}

# The JSON payload handed to the browser. Ships the node table rather than the
# segment table: the nodes are a fraction of the size and let the runtime
# synthesize any level of detail, whereas segments fix one level at build time.
.build_dynamic_config <- function(
    dendro, tbl, palette, max_leaves, budget_mode, min_leaf_px, priority,
    min_clade, shape, orientation, show_labels, label_budget, uirevision
) {
  # `labels` rows are in display order: row p holds leaf order[p] at x = p.
  # Invert that so a merge's leaf child can be resolved by hclust index.
  n <- nrow(dendro$labels)
  leaf_row <- integer(n)
  leaf_row[attr(dendro, "hclust")$order] <- seq_len(n)

  list(
    nodes = list(
      x = as.numeric(tbl$x),
      height = as.numeric(tbl$height),
      members = as.integer(tbl$members),
      span_lo = as.integer(tbl$span_lo),
      span_hi = as.integer(tbl$span_hi),
      left = as.integer(tbl$left),
      right = as.integer(tbl$right)
    ),
    # Two orderings, deliberately. `leaves` is indexed by hclust leaf index,
    # because that is how merge rows encode their leaf children. `labels` stays
    # in display order, because that is how ticks are addressed. Conflating the
    # two silently mislocates every leaf-terminated branch.
    leaves = list(
      x = as.numeric(leaf_row),
      y = as.numeric(dendro$labels$y[leaf_row])
    ),
    labels = as.character(dendro$labels$label),
    leaf_color = palette$index,
    palette = as.character(palette$colors),
    n = n,
    # The glyph floor is the lowest leaf, not zero: with `hang` the leaves sit
    # well above zero, and basing wedges at zero would stretch every one of
    # them through empty space below the tree.
    base = min(dendro$labels$y),
    max_leaves = max_leaves,
    budget_mode = budget_mode,
    min_leaf_px = min_leaf_px,
    priority = priority,
    min_clade = min_clade,
    shape = shape,
    orientation = orientation,
    show_labels = isTRUE(show_labels),
    label_budget = label_budget,
    label_cutoff_factor = 10,
    uirevision = uirevision,
    tag = .dendro_tag,
    n_clade_traces = length(palette$colors)
  )
}
