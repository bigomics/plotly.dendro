#' Add Dendrogram Merge Node Layer
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
  oriented <- orient_data(dendro, orientation = orientation)
  nodes <- oriented$nodes

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
