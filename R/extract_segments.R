#' Extract Dendrogram Segments
#'
#' Thin wrapper around the native geometry kernel returning only `segments`.
#'
#' @param hc An `hclust` object.
#'
#' @return A data frame with columns `x`, `y`, `xend`, `yend`.
extract_segments <- function(hc) {
  if (!inherits(hc, "hclust")) {
    stop("extract_segments() expects an hclust object.", call. = FALSE)
  }
  .native_dendro_data(hc, hang = NULL, nodes = FALSE)$segments
}
