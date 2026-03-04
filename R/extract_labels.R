#' Extract Leaf Labels
#'
#' @param hc An `hclust` object.
#' @param labels Optional custom labels.
#'
#' @return A data frame with columns `x`, `y`, `label`.
extract_labels <- function(hc, labels = NULL) {
  if (!inherits(hc, "hclust")) {
    stop("extract_labels() expects an hclust object.", call. = FALSE)
  }

  n <- length(hc$order)
  if (is.null(labels)) {
    if (is.null(hc$labels)) {
      labels <- as.character(seq_len(n))
    } else {
      labels <- hc$labels
    }
  }

  if (length(labels) != n) {
    stop("labels must have one entry per leaf.", call. = FALSE)
  }

  if (requireNamespace("ggdendro", quietly = TRUE)) {
    lbs <- .with_null_device(
      ggdendro::label(ggdendro::dendro_data(hc, type = "rectangle"))
    )
    ord <- order(lbs$x)
    lbs$label[ord] <- as.character(labels[hc$order])
    rownames(lbs) <- NULL
    return(lbs)
  }

  data.frame(
    x = seq_len(n),
    y = rep(0, n),
    label = as.character(labels[hc$order]),
    stringsAsFactors = FALSE
  )
}
