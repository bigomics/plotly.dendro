#' Build Tidy Dendrogram Data
#'
#' @param x Input data (`matrix`, `data.frame`, `dist`, or `hclust`).
#' @param distfun Distance function used when `x` is matrix-like.
#' @param linkagefun Linkage function used when `x` is matrix-like or `dist`.
#' @param labels Optional leaf labels.
#'
#' @return A list with `segments`, `labels`, and `nodes` data frames.
#' @export
dendro_data <- function(
    x,
    distfun = stats::dist,
    linkagefun = function(d) stats::hclust(d, method = "complete"),
    labels = NULL
) {
  hc <- as_hclust(x, distfun = distfun, linkagefun = linkagefun)

  out <- list(
    segments = extract_segments(hc),
    labels = extract_labels(hc, labels = labels),
    nodes = extract_nodes(hc)
  )

  attr(out, "hclust") <- hc
  out
}
