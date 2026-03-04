#' Build Plotly Layout For Dendrogram Data
#'
#' @param dendro A list with `segments`, `labels`, and `nodes`.
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#'
#' @return A plotly layout list.
dendro_layout <- function(dendro, orientation = "bottom") {
  .validate_orientation(orientation)

  labels <- dendro$labels
  if (orientation %in% c("bottom", "top")) {
    ord <- order(labels$x)
    axis_leaf <- list(
      tickvals = as.numeric(labels$x[ord]),
      ticktext = as.character(labels$label[ord]),
      tickmode = "array",
      showgrid = FALSE,
      zeroline = FALSE
    )
    axis_other <- list(showgrid = FALSE, zeroline = FALSE)
    return(list(xaxis = axis_leaf, yaxis = axis_other))
  }

  ord <- order(labels$y)
  axis_leaf <- list(
    tickvals = as.numeric(labels$y[ord]),
    ticktext = as.character(labels$label[ord]),
    tickmode = "array",
    showgrid = FALSE,
    zeroline = FALSE
  )
  axis_other <- list(showgrid = FALSE, zeroline = FALSE)
  list(xaxis = axis_other, yaxis = axis_leaf)
}
