#' Add Dendrogram Label Layer
#'
#' Adds a text trace placing each leaf label at its display position. Useful
#' when tick labels are unavailable or undesired (e.g. subplot embedding).
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
add_dendro_labels <- function(
    p,
    dendro,
    orientation = "bottom",
    ...,
    data = NULL,
    inherit = FALSE
) {
  .validate_orientation(orientation)

  layout <- p$x$layout
  has_tick_labels <- !is.null(layout$xaxis$ticktext) || !is.null(layout$yaxis$ticktext)
  if (isTRUE(has_tick_labels)) {
    warning("This plot already has axis tick labels; add_dendro_labels() may duplicate leaf labels.", call. = FALSE)
  }

  labels <- .orient_xy(dendro$labels, orientation)

  plotly::add_trace(
    p,
    data = labels,
    x = ~x,
    y = ~y,
    text = ~label,
    type = "scatter",
    mode = "text",
    showlegend = FALSE,
    inherit = inherit,
    ...
  )
}
