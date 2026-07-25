.bare_axis <- function() {
  list(showgrid = FALSE, zeroline = FALSE, ticks = "")
}

# Leaf axis: bare styling plus, when show_labels is TRUE, tick positions/text
# in display order (`values`/`labels` sorted by `values`).
.leaf_axis <- function(values, labels, show_labels) {
  axis <- .bare_axis()
  if (!show_labels) {
    axis$showticklabels <- FALSE
    return(axis)
  }
  ord <- order(values)
  axis$tickvals <- as.numeric(values[ord])
  axis$ticktext <- as.character(labels[ord])
  axis$tickmode <- "array"
  axis
}

#' Build Plotly Layout For Dendrogram Data
#'
#' Configures the leaf axis (tick labels at each leaf's display position)
#' and the height axis (bare, no ticks/grid) for one orientation.
#'
#' @param dendro A list with `segments`, `labels`, and `nodes`.
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#' @param show_labels Logical; if `FALSE`, omit tick labels from the leaf axis.
#'   Useful for subplot embedding.
#'
#' @return A plotly layout list.
dendro_layout <- function(dendro, orientation = "bottom", show_labels = TRUE) {
  .validate_orientation(orientation)

  labels <- dendro$labels
  if (orientation %in% c("bottom", "top")) {
    axis_leaf <- .leaf_axis(labels$x, labels$label, show_labels)
    return(list(xaxis = axis_leaf, yaxis = .bare_axis()))
  }

  axis_leaf <- .leaf_axis(labels$y, labels$label, show_labels)
  list(xaxis = .bare_axis(), yaxis = axis_leaf)
}
