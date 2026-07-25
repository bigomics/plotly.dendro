#' Extract Internal Merge Nodes
#'
#' Thin wrapper around the native geometry kernel returning only `nodes`.
#'
#' @param hc An `hclust` object.
#'
#' @return A data frame with columns `x`, `y`, `members`, and `height`.
extract_nodes <- function(hc) {
  if (!inherits(hc, "hclust")) {
    stop("extract_nodes() expects an hclust object.", call. = FALSE)
  }
  .native_dendro_data(hc, hang = NULL, nodes = TRUE)$nodes
}
