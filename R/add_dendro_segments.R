#' Add Dendrogram Segments Layer
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
  hc <- attr(dendro, "hclust")

  segs <- dendro$segments
  if (!is.null(hc)) {
    segs <- color_branches(segs, hc, color_threshold = color_threshold, colorscale = colorscale)
  } else if (!is.null(color_threshold) || !is.null(colorscale)) {
    warning("Branch coloring requires a dendro object with an 'hclust' attribute.", call. = FALSE)
  }

  oriented <- orient_data(
    list(segments = segs, labels = dendro$labels, nodes = dendro$nodes),
    orientation = orientation
  )
  segs <- oriented$segments

  line <- line %||% list()
  user_color <- line$color

  if (!("color" %in% names(segs))) {
    segs$color <- user_color %||% default_colorscale()[1]
  } else if (is.null(color_threshold) && !is.null(user_color)) {
    # color_branches ran (hc present) but no threshold — uniform color was set.
    # User's line$color overrides that uniform default.
    segs$color <- user_color
  }

  # Strip line$color from ... to avoid duplicate arg errors
  dots <- list(...)
  dots$line <- NULL

  for (col in unique(segs$color)) {
    chunk <- segs[segs$color == col, , drop = FALSE]
    # Merge user line props with per-branch color (branch color always wins)
    trace_line <- modifyList(line, list(color = col))
    p <- do.call(
      plotly::add_segments,
      c(list(
        p = p,
        data = chunk,
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

  p
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
