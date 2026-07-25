#' Add Dendrogram Segments Layer
#'
#' Adds the tree's branch/leaf segments as one or more `add_segments()`
#' traces — one uniform trace when branch coloring is inactive, or one
#' trace per resolved branch color otherwise.
#'
#' @param p A plotly object.
#' @param dendro A `dendro_data()` result.
#' @param orientation Dendrogram orientation.
#' @param color_threshold Height cutoff for branch colors.
#' @param colorscale Optional color vector.
#' @param line A named list of plotly line properties (e.g.
#'   `list(width = 2, dash = "dot", color = "black")`). The `color` element
#'   is used as the default segment color when `color_threshold` is not active;
#'   when branch coloring is active, `color` is overridden per branch.
#'   All other elements (`width`, `dash`, `shape`, etc.) always apply.
#' @param ... Passed to `plotly::add_segments()`.
#' @param data Optional plotly data.
#' @param inherit Plotly inheritance flag.
#'
#' @return A plotly object.
#' @importFrom utils modifyList
#' @export
add_dendro_segments <- function(
    p,
    dendro,
    orientation = "bottom",
    color_threshold = NULL,
    colorscale = NULL,
    line = NULL,
    ...,
    data = NULL,
    inherit = TRUE
) {
  .validate_orientation(orientation)
  resolved <- .resolve_segment_colors(
    dendro$segments,
    attr(dendro, "hclust"),
    color_threshold = color_threshold,
    colorscale = colorscale
  )

  # This layer consumes only segments. Avoid orienting/copying labels and
  # nodes, which are handled independently by their own layers.
  segs <- .orient_xy(resolved$segs, orientation)

  line <- line %||% list()
  uniform_color <- .finalize_uniform_color(resolved$uniform_color, line$color, segs)

  # Strip line$color from ... to avoid duplicate arg errors
  dots <- list(...)
  dots$line <- NULL

  if (!is.null(uniform_color)) {
    return(.add_segment_trace(p, segs, line, uniform_color, inherit, dots))
  }

  for (col in unique(segs$color)) {
    chunk <- segs[segs$color == col, , drop = FALSE]
    p <- .add_segment_trace(p, chunk, line, col, inherit, dots)
  }

  p
}

# Resolves per-segment branch coloring ahead of orientation/tracing: a NULL
# `color_threshold` with an hclust attribute means coloring is inactive
# (one uniform color); otherwise dispatches to color_branches() for
# per-cluster segment colors. Returns list(segs, uniform_color), where
# uniform_color is NULL when segs$color varies by branch.
.resolve_segment_colors <- function(segs, hc, color_threshold, colorscale) {
  uniform_color <- NULL
  if (!is.null(hc)) {
    if (is.null(color_threshold)) {
      colorscale <- .validate_colorscale(colorscale %||% default_colorscale())
      uniform_color <- colorscale[1]
    } else {
      segs <- color_branches(
        segs,
        hc,
        color_threshold = color_threshold,
        colorscale = colorscale
      )
    }
  } else if (!is.null(color_threshold) || !is.null(colorscale)) {
    warning("Branch coloring requires a dendro object with an 'hclust' attribute.", call. = FALSE)
  }
  list(segs = segs, uniform_color = uniform_color)
}

# A user-supplied line$color always overrides computed branch coloring;
# a segment table with no color column (canonical or hclust-less input)
# falls back to the base colorscale color.
.finalize_uniform_color <- function(uniform_color, user_color, segs) {
  if (is.null(uniform_color) && !("color" %in% names(segs))) {
    uniform_color <- default_colorscale()[1]
  }
  if (!is.null(uniform_color) && !is.null(user_color)) {
    uniform_color <- user_color
  }
  uniform_color
}

# Adds one add_segments() trace, merging `line` (user props) with the
# resolved per-trace `color` — branch color always wins over `line$color`
# for the trace it's applied to — plus any ...-forwarded `dots`.
.add_segment_trace <- function(p, data, line, color, inherit, dots) {
  trace_line <- modifyList(line, list(color = color))
  do.call(
    plotly::add_segments,
    c(list(
      p = p,
      data = data,
      x = ~x,
      y = ~y,
      xend = ~xend,
      yend = ~yend,
      line = trace_line,
      showlegend = FALSE,
      inherit = inherit
    ), dots)
  )
}

# Returns `a` unless it is NULL, in which case returns `b`.
`%||%` <- function(a, b) if (!is.null(a)) a else b
