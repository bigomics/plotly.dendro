#' Add Dendrogram Traces Without Layout Side-Effects
#'
#' Adds segment traces to a plotly object using a pre-computed
#' [dendro_data()] result. Unlike [add_dendro()], this function does
#' **not** modify the plotly layout — the caller controls axis
#' configuration. This is the recommended entry point for subplot
#' and dashboard composition.
#'
#' @param p A plotly object.
#' @param dendro A [dendro_data()] result.
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#' @param color_threshold Height cutoff for branch colors.
#' @param colorscale Optional color vector.
#' @param line A named list of plotly line properties (e.g.
#'   `list(width = 2, dash = "dot")`). See [add_dendro_segments()] for
#'   details on how `color` interacts with `color_threshold`.
#' @param ... Passed to [plotly::add_segments()].
#' @param data Optional plotly data.
#' @param inherit Plotly inheritance flag.
#'
#' @return A plotly object with segment traces added.
#' @export
add_dendro_traces <- function(
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
  add_dendro_segments(
    p,
    dendro,
    orientation = orientation,
    color_threshold = color_threshold,
    colorscale = colorscale,
    line = line,
    ...,
    data = data,
    inherit = inherit
  )
}
