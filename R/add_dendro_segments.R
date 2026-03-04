#' Add Dendrogram Segments Layer
#'
#' @param p A plotly object.
#' @param dendro A `dendro_data()` result.
#' @param orientation Dendrogram orientation.
#' @param color_threshold Height cutoff for branch colors.
#' @param colorscale Optional color vector.
#' @param ... Passed to `plotly::add_segments()`.
#' @param data Optional plotly data.
#' @param inherit Plotly inheritance flag.
#'
#' @return A plotly object.
#' @export
add_dendro_segments <- function(
    p,
    dendro,
    orientation = "bottom",
    color_threshold = NULL,
    colorscale = NULL,
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

  if (!("color" %in% names(segs))) {
    segs$color <- default_colorscale()[1]
  }

  for (col in unique(segs$color)) {
    chunk <- segs[segs$color == col, , drop = FALSE]
    p <- plotly::add_segments(
      p,
      data = chunk,
      x = ~x,
      y = ~y,
      xend = ~xend,
      yend = ~yend,
      line = list(color = col),
      showlegend = FALSE,
      inherit = inherit,
      ...
    )
  }

  p
}
