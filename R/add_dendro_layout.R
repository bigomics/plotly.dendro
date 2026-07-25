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
#' @param height_range Controls the height axis range. `"data"` (the default)
#'   leaves it unset so plotly autoranges to fit — with `hang` this yields the
#'   fitted axis that hierarchical-clustering plots conventionally use.
#'   `"full"` anchors the axis at zero, spanning `c(0, max_height)`. A
#'   `numeric(2)` clips to an explicit range, in native (pre-orientation)
#'   coordinates.
#'
#' @return A plotly object with layout applied.
#' @export
add_dendro_layout <- function(
    p,
    dendro,
    orientation = "bottom",
    show_labels = TRUE,
    height_range = c("data", "full")
) {
  .validate_orientation(orientation)
  od <- dendro
  od$labels <- .orient_xy(dendro$labels, orientation)
  do.call(
    plotly::layout,
    c(
      list(p),
      dendro_layout(
        od,
        orientation = orientation,
        show_labels = show_labels,
        height_range = .resolve_height_range(height_range, dendro)
      )
    )
  )
}

# "data" -> NULL (plotly autoranges); "full" -> zero-anchored; numeric(2) -> as
# given. Resolved against the unoriented geometry so callers always speak in
# native height units regardless of orientation.
.resolve_height_range <- function(height_range, dendro) {
  if (is.numeric(height_range)) {
    if (length(height_range) != 2L || anyNA(height_range) ||
        !all(is.finite(height_range))) {
      stop("height_range must be two finite numbers.", call. = FALSE)
    }
    return(sort(as.numeric(height_range)))
  }

  switch(
    match.arg(height_range, c("data", "full")),
    data = NULL,
    full = c(0, .max_dendro_height(dendro))
  )
}

.max_dendro_height <- function(dendro) {
  hc <- attr(dendro, "hclust")
  if (!is.null(hc)) {
    return(max(hc$height))
  }
  max(c(dendro$segments$y, dendro$segments$yend), na.rm = TRUE)
}
