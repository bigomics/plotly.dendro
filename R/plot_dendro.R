#' Create A Plotly Dendrogram
#'
#' @param x Input data (`matrix`, `data.frame`, `dist`, or `hclust`).
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#' @param labels Optional custom labels.
#' @param colorscale Optional color vector.
#' @param distfun Distance function.
#' @param linkagefun Linkage function.
#' @param color_threshold Branch color cutoff.
#' @param hovertext Optional hover text (reserved for future use).
#' @param hang Fraction of tree height for leaf hang (see [dendro_data()]).
#' @param line A named list of plotly line properties (e.g.
#'   `list(width = 2, dash = "dot")`). See [add_dendro_segments()] for details.
#' @param show_labels Logical; if `FALSE`, omit tick labels from the leaf axis.
#' @param width Optional widget width.
#' @param height Optional widget height.
#' @param source Source id for `event_data()`.
#' @param ... Passed to `add_dendro()`.
#'
#' @return A `plotly` htmlwidget with a `"dendro_data"` attribute.
#' @export
plot_dendro <- function(
    x,
    orientation = "bottom",
    labels = NULL,
    colorscale = NULL,
    distfun = stats::dist,
    linkagefun = function(d) stats::hclust(d, method = "complete"),
    color_threshold = NULL,
    hovertext = NULL,
    hang = NULL,
    line = NULL,
    show_labels = TRUE,
    width = NULL,
    height = NULL,
    source = "A",
    ...
) {
  p <- plotly::plot_ly(width = width, height = height, source = source)
  add_dendro(
    p,
    x,
    orientation = orientation,
    labels = labels,
    colorscale = colorscale,
    distfun = distfun,
    linkagefun = linkagefun,
    color_threshold = color_threshold,
    hovertext = hovertext,
    hang = hang,
    line = line,
    show_labels = show_labels,
    ...
  )
}
