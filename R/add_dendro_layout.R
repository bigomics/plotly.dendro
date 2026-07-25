#' Apply Dendrogram Layout To A Plotly Object
#'
#' Configures axis properties (tick labels, grid, zero-line) for a
#' dendrogram using a pre-computed [dendro_data()] result. Unlike
#' [add_dendro()], this function does **not** add any traces — use it
#' together with [add_dendro_traces()] for full compositional control.
#'
#' @param p A plotly object.
#' @param dendro A [dendro_data()] result.
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#' @param show_labels Logical; if `FALSE`, omit tick labels from the
#'   leaf axis. Useful for subplot embedding.
#'
#' @return A plotly object with layout applied.
#' @export
add_dendro_layout <- function(
    p,
    dendro,
    orientation = "bottom",
    show_labels = TRUE
) {
  .validate_orientation(orientation)
  od <- dendro
  od$labels <- .orient_xy(dendro$labels, orientation)
  do.call(
    plotly::layout,
    c(list(p), dendro_layout(od, orientation = orientation, show_labels = show_labels))
  )
}
