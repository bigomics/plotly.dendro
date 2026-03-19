#' Add A Full Dendrogram To An Existing Plotly Figure
#'
#' Computes [dendro_data()], adds segment traces via [add_dendro_traces()],
#' and applies axis layout via [add_dendro_layout()]. For full compositional
#' control, call those two functions directly with a pre-computed `dendro_data`
#' object.
#'
#' @param p A plotly object.
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
#' @param ... Passed to lower-level plotting calls.
#' @param data Optional plotly data.
#' @param inherit Plotly inheritance flag.
#'
#' @return A plotly object with a `"dendro_data"` attribute.
#' @export
add_dendro <- function(
    p,
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
    ...,
    data = NULL,
    inherit = TRUE
) {
  .validate_orientation(orientation)
  if (!is.null(hovertext)) {
    warning("hovertext is not yet implemented and will be ignored.", call. = FALSE)
  }

  d <- dendro_data(x, distfun = distfun, linkagefun = linkagefun, labels = labels, hang = hang)

  p <- add_dendro_traces(
    p,
    d,
    orientation = orientation,
    color_threshold = color_threshold,
    colorscale = colorscale,
    line = line,
    ...,
    data = data,
    inherit = inherit
  )

  p <- add_dendro_layout(p, d, orientation = orientation, show_labels = show_labels)

  attr(p, "dendro_data") <- d
  p
}
