#' Add A Full Dendrogram To An Existing Plotly Figure
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
#' @param ... Passed to lower-level plotting calls.
#' @param data Optional plotly data.
#' @param inherit Plotly inheritance flag.
#'
#' @return A plotly object.
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
    ...,
    data = NULL,
    inherit = TRUE
) {
  .validate_orientation(orientation)
  if (!is.null(hovertext)) {
    warning("hovertext is not yet implemented and will be ignored.", call. = FALSE)
  }

  d <- dendro_data(x, distfun = distfun, linkagefun = linkagefun, labels = labels)
  od <- orient_data(d, orientation = orientation)

  p <- add_dendro_segments(
    p,
    d,
    orientation = orientation,
    color_threshold = color_threshold,
    colorscale = colorscale,
    ...,
    data = data,
    inherit = inherit
  )

  do.call(plotly::layout, c(list(p), dendro_layout(od, orientation = orientation)))
}
