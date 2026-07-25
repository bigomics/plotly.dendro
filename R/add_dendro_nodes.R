#' Add Dendrogram Merge Node Layer
#'
#' Adds a marker trace at each internal merge node, hoverable with member
#' count and merge height. Requires `dendro` to have been built with
#' `dendro_data(..., nodes = TRUE)`.
#'
#' @param p A plotly object.
#' @param dendro A `dendro_data()` result.
#' @param orientation Dendrogram orientation.
#' @param ... Passed to `plotly::add_trace()`.
#' @param data Optional plotly data.
#' @param inherit Plotly inheritance flag.
#'
#' @return A plotly object.
#' @export
add_dendro_nodes <- function(
    p,
    dendro,
    orientation = "bottom",
    ...,
    data = NULL,
    inherit = FALSE
) {
  .validate_orientation(orientation)
  # This layer consumes only nodes. Avoid orienting/copying segments and
  # labels, which are handled independently by their own layers.
  nodes <- .orient_xy(dendro$nodes, orientation)

  plotly::add_trace(
    p,
    data = nodes,
    x = ~x,
    y = ~y,
    text = ~paste0("members: ", members, "<br>height: ", height),
    hoverinfo = "text",
    type = "scatter",
    mode = "markers",
    marker = list(size = 5, opacity = 0.6),
    showlegend = FALSE,
    inherit = inherit,
    ...
  )
}
