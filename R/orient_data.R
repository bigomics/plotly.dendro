.validate_orientation <- function(orientation) {
  valid <- c("bottom", "top", "left", "right")
  if (!(orientation %in% valid)) {
    stop("orientation must be one of: bottom, top, left, right.", call. = FALSE)
  }
}

.orient_xy <- function(df, orientation) {
  if (!all(c("x", "y") %in% names(df))) {
    return(df)
  }

  if (orientation == "bottom") {
    return(df)
  }

  out <- df
  if (orientation == "top") {
    out$y <- -df$y
    if ("yend" %in% names(df)) {
      out$yend <- -df$yend
    }
    return(out)
  }

  if (orientation == "left") {
    out$x <- df$y
    out$y <- df$x
    if ("xend" %in% names(df) && "yend" %in% names(df)) {
      out$xend <- df$yend
      out$yend <- df$xend
    }
    return(out)
  }

  # right
  out$x <- df$y
  out$y <- -df$x
  if ("xend" %in% names(df) && "yend" %in% names(df)) {
    out$xend <- df$yend
    out$yend <- -df$xend
  }
  out
}

#' Orient Dendrogram Coordinates
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
