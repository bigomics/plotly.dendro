#' Build Plotly Layout For Dendrogram Data
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
    ord <- order(labels$x)
    axis_leaf <- list(
      showgrid = FALSE,
      zeroline = FALSE,
      ticks = ""
    )
    if (show_labels) {
      axis_leaf$tickvals <- as.numeric(labels$x[ord])
      axis_leaf$ticktext <- as.character(labels$label[ord])
      axis_leaf$tickmode <- "array"
    } else {
      axis_leaf$showticklabels <- FALSE
    }
    axis_other <- list(showgrid = FALSE, zeroline = FALSE, ticks = "")
    return(list(xaxis = axis_leaf, yaxis = axis_other))
  }

  ord <- order(labels$y)
  axis_leaf <- list(
    showgrid = FALSE,
    zeroline = FALSE,
    ticks = ""
  )
  if (show_labels) {
    axis_leaf$tickvals <- as.numeric(labels$y[ord])
    axis_leaf$ticktext <- as.character(labels$label[ord])
    axis_leaf$tickmode <- "array"
  } else {
    axis_leaf$showticklabels <- FALSE
  }
  axis_other <- list(showgrid = FALSE, zeroline = FALSE, ticks = "")
  list(xaxis = axis_other, yaxis = axis_leaf)
}
