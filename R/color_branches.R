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

# Maps each leaf to a color: uniform when `color_threshold` is NULL,
# per-`cutree()`-cluster otherwise (one color per leaf when threshold <= 0).
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

# Rounds (x, y) to a stable string key so segment endpoints that should
# coincide can be matched by exact string equality instead of float comparison.
.coord_key <- function(x, y, digits = 10L) {
  paste0(format(round(x, digits), nsmall = digits), "|", format(round(y, digits), nsmall = digits))
}

# Recursively resolves which leaf indices sit beneath a node key, memoizing
# per node in `cache` so shared subtrees aren't re-walked.
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

# Pairs each horizontal segment with the vertical segment its endpoint
# feeds into (matched by coordinate key), reconstructing parent/child edges
# from an arbitrary (possibly reordered) segment table.
.match_horiz_vert_edges <- function(segs, horiz_idx, vert_idx) {
  vert_by_start <- split(vert_idx, .coord_key(segs$x[vert_idx], segs$y[vert_idx]))

  edge_h <- integer(0)
  edge_v <- integer(0)
  edge_parent <- character(0)
  edge_child <- character(0)
  for (h in horiz_idx) {
    candidates <- vert_by_start[[.coord_key(segs$xend[h], segs$yend[h])]]
    if (is.null(candidates) || length(candidates) == 0) {
      next
    }
    v <- candidates[1]
    edge_h <- c(edge_h, h)
    edge_v <- c(edge_v, v)
    edge_parent <- c(edge_parent, .coord_key(segs$x[h], segs$y[h]))
    edge_child <- c(edge_child, .coord_key(segs$xend[h], segs$yend[v]))
  }
  list(edge_h = edge_h, edge_v = edge_v, edge_parent = edge_parent, edge_child = edge_child)
}

# Coloring fallback for arbitrary (reordered or subset) segment tables that
# fail `.native_color_segments()`'s canonical-coordinate check: reconstructs
# tree topology from segment coordinates alone, then colors each edge by its
# descendant leaves' unanimous color (or the base color if they disagree).
.color_segments_by_topology <- function(segs, leaf_cols, colorscale, hc) {
  tol <- 1e-10
  colors <- rep(colorscale[1], nrow(segs))

  horiz_idx <- which(abs(segs$y - segs$yend) <= tol & abs(segs$x - segs$xend) > tol)
  vert_idx <- which(abs(segs$x - segs$xend) <= tol & abs(segs$y - segs$yend) > tol)
  if (length(horiz_idx) == 0 || length(vert_idx) == 0) {
    return(colors)
  }

  edges <- .match_horiz_vert_edges(segs, horiz_idx, vert_idx)
  if (length(edges$edge_h) == 0) {
    return(colors)
  }

  children_by_parent <- split(edges$edge_child, edges$edge_parent)
  leaf_positions <- sort(unique(extract_labels(hc)$x))
  if (length(leaf_positions) != length(leaf_cols)) {
    leaf_positions <- seq_along(leaf_cols)
  }
  cache <- new.env(parent = emptyenv())

  for (i in seq_along(edges$edge_h)) {
    leaf_ids <- .descendant_leaf_ids(
      edges$edge_child[i],
      children_by_parent = children_by_parent,
      leaf_positions = leaf_positions,
      tol = tol,
      cache = cache
    )
    edge_cols <- unique(leaf_cols[leaf_ids])
    col <- if (length(edge_cols) == 1) edge_cols[1] else colorscale[1]
    colors[edges$edge_h[i]] <- col
    colors[edges$edge_v[i]] <- col
  }

  colors
}

#' Color Dendrogram Segments
#'
#' Assigns a `color` per segment by cluster membership at `color_threshold`.
#' Uses the native branch-ID kernel when `segs` has canonical coordinates
#' (the common case), falling back to an R topology reconstruction for
#' reordered or subset segment tables.
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
  colorscale <- .validate_colorscale(colorscale)

  if (!is.data.frame(segs) || !all(c("x", "y", "xend", "yend") %in% names(segs))) {
    stop("segs must be a data.frame with x, y, xend, yend columns.", call. = FALSE)
  }
  if (!inherits(hc, "hclust")) {
    stop("hc must be an hclust object.", call. = FALSE)
  }

  if (is.null(color_threshold)) {
    segs$color <- rep.int(colorscale[1], nrow(segs))
    return(segs)
  }

  leaf_cols <- .leaf_cluster_colors(hc, color_threshold = color_threshold, colorscale = colorscale)
  colors <- .native_color_segments(
    segs,
    hc,
    leaf_cols = leaf_cols,
    colorscale = colorscale
  )
  if (is.null(colors)) {
    colors <- .color_segments_by_topology(
      segs,
      leaf_cols = leaf_cols,
      colorscale = colorscale,
      hc = hc
    )
  }
  segs$color <- colors
  segs
}
