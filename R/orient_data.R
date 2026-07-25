# Errors unless `orientation` is one of the four supported layout directions.
.validate_orientation <- function(orientation) {
  valid <- c("bottom", "top", "left", "right")
  if (!(orientation %in% valid)) {
    stop("orientation must be one of: bottom, top, left, right.", call. = FALSE)
  }
}

# Rotates/flips a single x/y(/xend/yend) data frame for one orientation.
# "bottom" is the native layout (no-op); "top" flips y; "left"/"right" swap
# x and y, "right" additionally flipping the new y. Leaves `df` unchanged if
# it lacks x/y columns (e.g. an empty nodes frame).
.orient_xy <- function(df, orientation) {
  if (orientation == "bottom" || !all(c("x", "y") %in% names(df))) {
    return(df)
  }

  has_end <- all(c("xend", "yend") %in% names(df))
  out <- df
  switch(
    orientation,
    top = {
      out$y <- -df$y
      if ("yend" %in% names(df)) {
        out$yend <- -df$yend
      }
    },
    left = {
      out$x <- df$y
      out$y <- df$x
      if (has_end) {
        out$xend <- df$yend
        out$yend <- df$xend
      }
    },
    right = {
      out$x <- df$y
      out$y <- -df$x
      if (has_end) {
        out$xend <- df$yend
        out$yend <- -df$xend
      }
    }
  )
  out
}

#' Orient Dendrogram Coordinates
#'
#' Rotates/flips `segments`, `labels`, and `nodes` together for a given
#' orientation. Prefer orienting only the parts a caller needs (see
#' [add_dendro_segments()], [add_dendro_labels()]) — this whole-list
#' transform copies every component.
#'
#' @param dendro A list with `segments`, `labels`, and `nodes`.
#' @param orientation One of `"bottom"`, `"top"`, `"left"`, or `"right"`.
#'
#' @return The transformed `dendro` list.
orient_data <- function(dendro, orientation = "bottom") {
  .validate_orientation(orientation)

  required <- c("segments", "labels", "nodes")
  if (!is.list(dendro) || !all(required %in% names(dendro))) {
    stop("dendro must be a list with segments, labels, and nodes.", call. = FALSE)
  }

  out <- dendro
  out$segments <- .orient_xy(dendro$segments, orientation)
  out$labels <- .orient_xy(dendro$labels, orientation)
  out$nodes <- .orient_xy(dendro$nodes, orientation)
  out
}
