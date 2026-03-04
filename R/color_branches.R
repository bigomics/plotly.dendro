#' Default Dendrogram Colorscale
#'
#' @return Character vector of 8 RGB colors.
default_colorscale <- function() {
  c(
    "rgb(0,116,217)",
    "rgb(35,205,205)",
    "rgb(61,153,112)",
    "rgb(40,35,35)",
    "rgb(133,20,75)",
    "rgb(255,65,54)",
    "rgb(255,255,255)",
    "rgb(255,220,0)"
  )
}

.leaf_cluster_colors <- function(hc, color_threshold, colorscale) {
  n <- length(hc$order)
  if (is.null(color_threshold)) {
    return(rep(colorscale[1], n))
  }

  h <- color_threshold

  clusters <- stats::cutree(hc, h = h)
  ordered_clusters <- clusters[hc$order]

  # Special-case threshold at or below 0 so each leaf can receive its own color.
  if (!is.null(color_threshold) && color_threshold <= 0) {
    ordered_clusters <- seq_len(n)
  }

  ids <- sort(unique(ordered_clusters))
  lookup <- stats::setNames(colorscale[((seq_along(ids) - 1L) %% length(colorscale)) + 1L], ids)
  unname(lookup[as.character(ordered_clusters)])
}

.coord_key <- function(x, y, digits = 10L) {
  paste0(format(round(x, digits), nsmall = digits), "|", format(round(y, digits), nsmall = digits))
}

.descendant_leaf_ids <- function(node_key, children_by_parent, leaf_positions, tol, cache) {
  if (exists(node_key, envir = cache, inherits = FALSE)) {
    return(get(node_key, envir = cache, inherits = FALSE))
  }

  parts <- strsplit(node_key, "\\|", fixed = FALSE)[[1]]
  x <- as.numeric(parts[1])
  y <- as.numeric(parts[2])

  if (abs(y) <= tol) {
    ids <- which.min(abs(leaf_positions - x))
    assign(node_key, ids, envir = cache)
    return(ids)
  }

  children <- children_by_parent[[node_key]]
  if (is.null(children) || length(children) == 0) {
    ids <- which.min(abs(leaf_positions - x))
    assign(node_key, ids, envir = cache)
    return(ids)
  }

  ids <- unique(unlist(lapply(
    children,
    .descendant_leaf_ids,
    children_by_parent = children_by_parent,
    leaf_positions = leaf_positions,
    tol = tol,
    cache = cache
  )))
  assign(node_key, ids, envir = cache)
  ids
}

.color_segments_by_topology <- function(segs, leaf_cols, colorscale, hc) {
  tol <- 1e-10
  n <- nrow(segs)
  colors <- rep(colorscale[1], n)

  horiz_idx <- which(abs(segs$y - segs$yend) <= tol & abs(segs$x - segs$xend) > tol)
  vert_idx <- which(abs(segs$x - segs$xend) <= tol & abs(segs$y - segs$yend) > tol)
  if (length(horiz_idx) == 0 || length(vert_idx) == 0) {
    return(colors)
  }

  vert_start_keys <- .coord_key(segs$x[vert_idx], segs$y[vert_idx])
  vert_by_start <- split(vert_idx, vert_start_keys)

  edge_h <- integer(0)
  edge_v <- integer(0)
  edge_parent <- character(0)
  edge_child <- character(0)

  for (h in horiz_idx) {
    child_start <- .coord_key(segs$xend[h], segs$yend[h])
    candidates <- vert_by_start[[child_start]]
    if (is.null(candidates) || length(candidates) == 0) {
      next
    }
    v <- candidates[1]
    edge_h <- c(edge_h, h)
    edge_v <- c(edge_v, v)
    edge_parent <- c(edge_parent, .coord_key(segs$x[h], segs$y[h]))
    edge_child <- c(edge_child, .coord_key(segs$xend[h], segs$yend[v]))
  }

  if (length(edge_h) == 0) {
    return(colors)
  }

  children_by_parent <- split(edge_child, edge_parent)
  leaf_positions <- sort(unique(extract_labels(hc)$x))
  if (length(leaf_positions) != length(leaf_cols)) {
    leaf_positions <- seq_along(leaf_cols)
  }
  cache <- new.env(parent = emptyenv())

  for (i in seq_along(edge_h)) {
    leaf_ids <- .descendant_leaf_ids(
      edge_child[i],
      children_by_parent = children_by_parent,
      leaf_positions = leaf_positions,
      tol = tol,
      cache = cache
    )
    edge_cols <- unique(leaf_cols[leaf_ids])
    col <- if (length(edge_cols) == 1) edge_cols[1] else colorscale[1]
    colors[edge_h[i]] <- col
    colors[edge_v[i]] <- col
  }

  colors
}

#' Color Dendrogram Segments
#'
#' @param segs Segment data frame.
#' @param hc An `hclust` object.
#' @param color_threshold Height cutoff for color grouping.
#' @param colorscale Optional color vector.
#'
#' @return `segs` with a `color` column.
color_branches <- function(segs, hc, color_threshold = NULL, colorscale = NULL) {
  if (is.null(colorscale)) {
    colorscale <- default_colorscale()
  }

  if (!is.data.frame(segs) || !all(c("x", "y", "xend", "yend") %in% names(segs))) {
    stop("segs must be a data.frame with x, y, xend, yend columns.", call. = FALSE)
  }
  if (!inherits(hc, "hclust")) {
    stop("hc must be an hclust object.", call. = FALSE)
  }

  leaf_cols <- .leaf_cluster_colors(hc, color_threshold = color_threshold, colorscale = colorscale)
  segs$color <- .color_segments_by_topology(segs, leaf_cols = leaf_cols, colorscale = colorscale, hc = hc)
  segs
}
