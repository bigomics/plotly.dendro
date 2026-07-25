#' @useDynLib plotly.dendro, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

# Typed-empty nodes data frame, returned when node materialization is
# skipped (see `dendro_data(nodes = FALSE)`).
.empty_nodes <- function() {
  data.frame(
    x = numeric(),
    y = numeric(),
    members = integer(),
    height = numeric(),
    stringsAsFactors = FALSE
  )
}

# Resolve leaf labels for the native call: user-supplied, then hc$labels,
# then positional fallback "1", "2", ....
.resolve_dendro_labels <- function(hc, labels) {
  n <- length(hc$order)
  if (is.null(labels)) {
    labels <- hc$labels
    if (is.null(labels)) {
      labels <- as.character(seq_len(n))
    }
  }
  if (length(labels) != n) {
    stop("labels must have one entry per leaf.", call. = FALSE)
  }
  as.character(labels)
}

.integer_field_error <- function(name) {
  sprintf("Invalid hclust: %s must contain finite integer values.", name)
}

# A numeric vector/matrix is "integer-valued" if every element is finite,
# whole, and within R's representable integer range.
.validate_integer_valued <- function(x, name) {
  if (any(!is.finite(x)) || any(x != trunc(x)) ||
      any(x < -.Machine$integer.max) || any(x > .Machine$integer.max)) {
    stop(.integer_field_error(name), call. = FALSE)
  }
}

.validate_field_dimensions <- function(x, name, dimensions) {
  if (!is.null(dimensions) && (!is.matrix(x) || !identical(dim(x), dimensions))) {
    stop(sprintf("Invalid hclust: %s has invalid dimensions.", name), call. = FALSE)
  }
}

# Coerce a numeric hclust field (merge or order) to integer, validating
# finiteness, whole-valuedness, and (for merge) matrix dimensions first.
.normalize_integer_field <- function(x, name, dimensions = NULL) {
  if (!is.numeric(x) || anyNA(x)) {
    stop(.integer_field_error(name), call. = FALSE)
  }
  .validate_field_dimensions(x, name, dimensions)
  if (typeof(x) == "integer") {
    return(x)
  }
  .validate_integer_valued(x, name)
  out <- as.integer(x)
  if (!is.null(dimensions)) {
    dim(out) <- dimensions
    dimnames(out) <- dimnames(x)
  }
  out
}

.validate_merge_matrix <- function(merge) {
  if (!is.matrix(merge) || ncol(merge) != 2L || nrow(merge) < 1L) {
    stop(
      "Invalid hclust: merge must be a matrix with at least one row and two columns.",
      call. = FALSE
    )
  }
}

.validate_height_field <- function(height, n_merge) {
  if (!is.numeric(height) || length(height) != n_merge) {
    stop(
      "Invalid hclust: height must have one numeric value per merge row.",
      call. = FALSE
    )
  }
  as.numeric(height)
}

.validate_order_field <- function(order, n_leaf) {
  if (!is.numeric(order) || length(order) != n_leaf) {
    stop(
      "Invalid hclust: order must have one numeric entry per leaf.",
      call. = FALSE
    )
  }
}

.validate_labels_length <- function(labels, n_leaf) {
  if (!is.null(labels) && length(labels) != n_leaf) {
    stop("Invalid hclust: labels must have one entry per leaf.", call. = FALSE)
  }
}

# R-level normalization boundary before crossing into C++: coerces hclust
# fields to the shapes/types the native geometry kernel trusts without
# re-checking. Structural safety (subtree ordering, parent counts, etc.)
# is validated natively in dendro_geometry.cpp.
.normalize_hclust_inputs <- function(hc) {
  if (!inherits(hc, "hclust")) {
    stop("Expected an hclust object.", call. = FALSE)
  }

  .validate_merge_matrix(hc$merge)
  merge <- .normalize_integer_field(
    hc$merge,
    "merge",
    dimensions = c(nrow(hc$merge), 2L)
  )
  n_merge <- nrow(merge)
  n_leaf <- n_merge + 1L

  height <- .validate_height_field(hc$height, n_merge)
  .validate_order_field(hc$order, n_leaf)
  order <- .normalize_integer_field(hc$order, "order")
  .validate_labels_length(hc$labels, n_leaf)

  list(merge = merge, height = height, order = order)
}

# Fraction of the tree height at which to hang leaves, or -1 to disable
# hanging (dropping all leaves to y = 0). See `dendro_data()`'s `hang` param.
.leaf_drop <- function(height, hang) {
  if (is.null(hang) || hang < 0) {
    return(-1)
  }
  hang * height[length(height)]
}

# Normalizes `hc` and dispatches to the compiled geometry kernel
# (`cpp_hclust_geometry()`), which builds segments/labels/nodes in one pass.
.native_dendro_data <- function(hc, labels = NULL, hang = NULL, nodes = TRUE) {
  inputs <- .normalize_hclust_inputs(hc)
  labels <- .resolve_dendro_labels(hc, labels)
  cpp_hclust_geometry(
    merge = inputs$merge,
    height = inputs$height,
    order = inputs$order,
    labels = labels,
    leaf_drop = .leaf_drop(inputs$height, hang),
    need_nodes = nodes
  )
}

.validate_colorscale <- function(colorscale) {
  if (!is.character(colorscale) || length(colorscale) == 0L ||
      anyNA(colorscale) || any(!nzchar(colorscale))) {
    stop("colorscale must be a non-empty character vector of colors.", call. = FALSE)
  }
  colorscale
}

# Propagates leaf colors up the tree natively, returning colors per segment
# only when `segs` has canonical (unreordered) coordinates; NULL otherwise,
# signaling the caller to fall back to `.color_segments_by_topology()`.
.native_color_segments <- function(segs, hc, leaf_cols, colorscale) {
  inputs <- .normalize_hclust_inputs(hc)
  palette <- unique(c(colorscale[1], leaf_cols))
  leaf_ids <- match(leaf_cols, palette)
  ids <- cpp_hclust_branch_ids(
    merge = inputs$merge,
    height = inputs$height,
    order = inputs$order,
    segment_x = segs$x,
    segment_y = segs$y,
    segment_xend = segs$xend,
    segment_yend = segs$yend,
    leaf_color_ids = leaf_ids
  )
  if (is.null(ids)) {
    return(NULL)
  }
  palette[ids]
}
