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
#' @param height_range Optional numeric range for the height axis, in native
#'   (pre-orientation) coordinates. `NULL` leaves the axis unset so plotly
#'   autoranges.
#'
#' @return A plotly layout list.
dendro_layout <- function(
    dendro,
    orientation = "bottom",
    show_labels = TRUE,
    height_range = NULL
) {
  .validate_orientation(orientation)

  axis_height <- .height_axis(height_range, orientation)
  labels <- dendro$labels
  if (orientation %in% c("bottom", "top")) {
    axis_leaf <- .leaf_axis(labels$x, labels$label, show_labels)
    return(list(xaxis = axis_leaf, yaxis = axis_height))
  }

  axis_leaf <- .leaf_axis(labels$y, labels$label, show_labels)
  list(xaxis = axis_height, yaxis = axis_leaf)
}

# Height axis: bare styling, plus an explicit range when one was requested.
# The range arrives in native coordinates, so it needs the same flip that
# `.orient_xy()` applies to the data — only "top" negates the height axis.
.height_axis <- function(height_range, orientation) {
  axis <- .bare_axis()
  if (is.null(height_range)) {
    return(axis)
  }
  axis$range <- if (orientation == "top") {
    c(-height_range[2L], -height_range[1L])
  } else {
    height_range
  }
  axis$autorange <- FALSE
  axis
}
