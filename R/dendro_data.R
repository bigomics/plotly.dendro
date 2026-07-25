#' Build Tidy Dendrogram Data
#'
#' Coerces `x` to an `hclust` tree and computes its plot-ready geometry
#' (segments, leaf labels, and optionally internal nodes) via the native
#' geometry kernel in a single compiled pass.
#'
#' @param x Input data (`matrix`, `data.frame`, `dist`, or `hclust`).
#' @param distfun Distance function used when `x` is matrix-like.
#' @param linkagefun Linkage function used when `x` is matrix-like or `dist`.
#' @param labels Optional leaf labels.
#' @param hang Non-negative fraction of the tree height at which leaf segments
#'   are hung below their merge point, or a negative value to drop leaves to
#'   zero. `NULL` (the default) leaves segments unchanged.
#' @param nodes Logical; materialize internal merge nodes. Defaults to `TRUE`
#'   for backward compatibility. When `FALSE`, the native kernel skips node
#'   construction entirely (not just discarding it after the fact) and the
#'   result retains a typed empty `nodes` data frame; callers that only need
#'   `segments`/`labels` (e.g. [add_dendro_segments()], [add_dendro_layout()])
#'   should pass `FALSE` to avoid paying for node geometry they don't use.
#'
#' @return A list with `segments`, `labels`, and `nodes` data frames.
#' @export
dendro_data <- function(
    x,
    distfun = stats::dist,
    linkagefun = function(d) stats::hclust(d, method = "complete"),
    labels = NULL,
    hang = NULL,
    nodes = TRUE
) {
  hang <- .validate_hang(hang)
  if (!is.logical(nodes) || length(nodes) != 1L || is.na(nodes)) {
    stop("nodes must be TRUE or FALSE.", call. = FALSE)
  }
  hc <- as_hclust(x, distfun = distfun, linkagefun = linkagefun)

  out <- .native_dendro_data(
    hc,
    labels = labels,
    hang = hang,
    nodes = nodes
  )

  attr(out, "hclust") <- hc
  out
}

# NULL passes through unchanged; otherwise must be a single finite number.
.validate_hang <- function(hang) {
  if (is.null(hang)) {
    return(NULL)
  }

  if (!is.numeric(hang) || length(hang) != 1L || is.na(hang) || !is.finite(hang)) {
    stop("hang must be NULL or a single finite numeric value.", call. = FALSE)
  }

  hang
}
