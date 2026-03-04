.member_dend <- function(x) {
  r <- attr(x, "x.member")
  if (!is.null(r)) {
    return(r)
  }
  r <- attr(x, "members")
  if (!is.null(r)) {
    return(r)
  }
  1L
}

.mid_dend <- function(x) {
  mid <- attr(x, "midpoint")
  if (is.null(mid)) {
    return(0)
  }
  mid
}

.plot_node_limit <- function(x1, x2, subtree) {
  inner <- !stats::is.leaf(subtree) && (x1 != x2)
  if (inner) {
    k <- length(subtree)
    m_top <- .member_dend(subtree)
    limit <- numeric(k + 1)
    limit[1] <- x1
    xx1 <- x1
    for (idx in seq_len(k)) {
      m <- .member_dend(subtree[[idx]])
      xx1 <- xx1 + (x2 - x1) * m / m_top
      limit[idx + 1] <- xx1
    }
  } else {
    limit <- c(x1, x2)
  }

  list(x = x1 + .mid_dend(subtree), limit = limit)
}

.walk_dendrogram <- function(subtree, x1, x2, out) {
  node <- .plot_node_limit(x1, x2, subtree)
  x_top <- node$x

  if (stats::is.leaf(subtree) || x1 == x2) {
    return(out)
  }

  y_top <- attr(subtree, "height")
  if (is.null(y_top)) {
    y_top <- 0
  }

  out$nodes <- rbind(
    out$nodes,
    data.frame(
      x = x_top,
      y = y_top,
      members = .member_dend(subtree),
      height = y_top,
      stringsAsFactors = FALSE
    )
  )

  for (idx in seq_along(subtree)) {
    child <- subtree[[idx]]
    x_bot <- node$limit[idx] + .mid_dend(child)
    y_bot <- attr(child, "height")
    if (is.null(y_bot)) {
      y_bot <- 0
    }

    out$segments <- rbind(
      out$segments,
      data.frame(x = x_top, y = y_top, xend = x_bot, yend = y_top, stringsAsFactors = FALSE),
      data.frame(x = x_bot, y = y_top, xend = x_bot, yend = y_bot, stringsAsFactors = FALSE)
    )

    out <- .walk_dendrogram(child, node$limit[idx], node$limit[idx + 1], out)
  }

  out
}

.extract_tree_components <- function(hc) {
  dend <- stats::as.dendrogram(hc)
  n_leaves <- .member_dend(dend)

  out <- list(
    segments = data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric()),
    nodes = data.frame(x = numeric(), y = numeric(), members = numeric(), height = numeric())
  )

  out <- .walk_dendrogram(dend, 1, n_leaves, out)
  out
}

#' Extract Dendrogram Segments
#'
#' @param hc An `hclust` object.
#'
#' @return A data frame with columns `x`, `y`, `xend`, `yend`.
extract_segments <- function(hc) {
  if (!inherits(hc, "hclust")) {
    stop("extract_segments() expects an hclust object.", call. = FALSE)
  }

  if (requireNamespace("ggdendro", quietly = TRUE)) {
    segs <- .with_null_device(
      ggdendro::segment(ggdendro::dendro_data(hc, type = "rectangle"))
    )
    rownames(segs) <- NULL
    return(segs)
  }

  segs <- .extract_tree_components(hc)$segments
  rownames(segs) <- NULL
  segs
}
