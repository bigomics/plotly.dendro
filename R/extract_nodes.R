#' Extract Internal Merge Nodes
#'
#' @param hc An `hclust` object.
#'
#' @return A data frame with columns `x`, `y`, `members`, and `height`.
extract_nodes <- function(hc) {
  if (!inherits(hc, "hclust")) {
    stop("extract_nodes() expects an hclust object.", call. = FALSE)
  }

  if (requireNamespace("ggdendro", quietly = TRUE)) {
    segs <- extract_segments(hc)
    horiz <- segs[segs$y == segs$yend & segs$x != segs$xend, c("x", "y")]
    nodes_xy <- unique(horiz)
    nodes_xy <- nodes_xy[order(nodes_xy$y, nodes_xy$x), , drop = FALSE]
    rownames(nodes_xy) <- NULL

    return(data.frame(
      x = nodes_xy$x,
      y = nodes_xy$y,
      members = NA_integer_,
      height = nodes_xy$y,
      stringsAsFactors = FALSE
    ))
  }

  nodes <- .extract_tree_components(hc)$nodes
  rownames(nodes) <- NULL
  nodes
}
