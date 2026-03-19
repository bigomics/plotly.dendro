#' Build Tidy Dendrogram Data
#'
#' @param x Input data (`matrix`, `data.frame`, `dist`, or `hclust`).
#' @param distfun Distance function used when `x` is matrix-like.
#' @param linkagefun Linkage function used when `x` is matrix-like or `dist`.
#' @param labels Optional leaf labels.
#' @param hang Non-negative fraction of the tree height at which leaf segments
#'   are hung below their merge point, or a negative value to drop leaves to
#'   zero. `NULL` (the default) leaves segments unchanged.
#'
#' @return A list with `segments`, `labels`, and `nodes` data frames.
#' @export
dendro_data <- function(
    x,
    distfun = stats::dist,
    linkagefun = function(d) stats::hclust(d, method = "complete"),
    labels = NULL,
    hang = NULL
) {
  hang <- .validate_hang(hang)
  hc <- as_hclust(x, distfun = distfun, linkagefun = linkagefun)

  out <- list(
    segments = extract_segments(hc),
    labels = extract_labels(hc, labels = labels),
    nodes = extract_nodes(hc)
  )

  if (!is.null(hang)) {
    out <- .apply_hang(out, hc, hang)
  }

  attr(out, "hclust") <- hc
  out
}

.validate_hang <- function(hang) {
  if (is.null(hang)) {
    return(NULL)
  }

  if (!is.numeric(hang) || length(hang) != 1L || is.na(hang) || !is.finite(hang)) {
    stop("hang must be NULL or a single finite numeric value.", call. = FALSE)
  }

  hang
}

#' Apply Hang to Dendrogram Segments and Labels
#'
#' Adjusts leaf-segment endpoints and label y-positions so that leaves hang
#' below their merge point by a fraction of the tree height.
#'
#' @param dd A dendro_data list with `segments` and `labels`.
#' @param hc The `hclust` object used to build `dd`.
#' @param hang Non-negative fraction of max height at which leaves hang, or
#'   negative to drop leaves to zero.
#'
#' @return The modified `dd` list.
#' @keywords internal
#' @noRd
.apply_hang <- function(dd, hc, hang) {
  if (length(hc$height) == 0) return(dd)

  segs <- dd$segments
  labs <- dd$labels

  tol <- 1e-10
  is_leaf <- (abs(segs$x - segs$xend) < tol) & (abs(segs$yend) < tol)

  if (!any(is_leaf)) return(dd)

  max_h <- max(hc$height)

  if (hang < 0) {
    # Negative hang: leaves drop to zero (already at zero), nothing to change
    return(dd)
  }

  # Positive or zero hang: lower leaf endpoints by hang * max_h from their
  # parent merge height
  drop <- hang * max_h
  new_yend <- pmax(segs$y[is_leaf] - drop, 0)
  segs$yend[is_leaf] <- new_yend

  # Update label y-positions to match new leaf endpoints
  leaf_x <- segs$xend[is_leaf]
  leaf_yend <- segs$yend[is_leaf]
  idx <- match(labs$x, leaf_x)
  hit <- !is.na(idx)
  labs$y[hit] <- leaf_yend[idx[hit]]

  dd$segments <- segs
  dd$labels <- labs
  dd
}
