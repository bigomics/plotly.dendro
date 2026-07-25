#' Extract Leaf Labels
#'
#' Thin wrapper around the native geometry kernel returning only `labels`.
#'
#' @param hc An `hclust` object.
#' @param labels Optional custom labels.
#'
#' @return A data frame with columns `x`, `y`, `label`.
extract_labels <- function(hc, labels = NULL) {
  if (!inherits(hc, "hclust")) {
    stop("extract_labels() expects an hclust object.", call. = FALSE)
  }
  .native_dendro_data(
    hc,
    labels = labels,
    hang = NULL,
    nodes = FALSE
  )$labels
}
