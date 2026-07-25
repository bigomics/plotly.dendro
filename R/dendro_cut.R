#' Reduce A Dendrogram To A Budgeted Set Of Visible Clades
#'
#' Collapses a dendrogram to at most `max_leaves` visible clades, so that a
#' dense tree can be drawn without overplotting. The result is itself a
#' [dendro_data()]-shaped object, so every `add_dendro_*()` layer composes with
#' it unchanged — a cut is a transform on geometry, not a rendering mode.
#'
#' The cut is a priority-queue peel from the root: starting from the whole tree
#' as a single clade, the highest-priority clade is repeatedly expanded into its
#' two children until the budget is reached. Because expanding only ever
#' replaces a clade by its own children, the expanded set is guaranteed to be a
#' valid top-down-closed cut for *any* priority function, including non-monotone
#' ones.
#'
#' @param dendro A [dendro_data()] result carrying an `"hclust"` attribute.
#' @param max_leaves Maximum number of visible clades. `Inf` (or any value at
#'   or above the leaf count) returns `dendro` unchanged, so callers may apply
#'   this unconditionally.
#' @param priority How to spend the budget. One of `"members"` (expand the
#'   largest clade first — the default), `"height"` (expand the tallest merge
#'   first, equivalent to `scipy`'s `truncate_mode = "lastp"`), or `"hybrid"`
#'   (`height * log1p(members)`). May also be a function of
#'   `(members, height)` returning a numeric vector, for callers who want to
#'   drive the cut from their own score.
#' @param min_clade Stop subdividing clades that already hold fewer than this
#'   many leaves, spending the remaining budget on larger structure instead.
#'   Note this does *not* eliminate single-leaf clades: a hierarchical
#'   clustering routinely attaches an outlier leaf at a high merge, so
#'   splitting a large clade legitimately yields a one-member sibling. Those
#'   singletons are data, not waste.
#'
#' @return A `dendro_data()`-shaped list whose `segments` cover only the
#'   expanded part of the tree, plus a `clades` data frame describing the
#'   collapsed frontier with columns `x`, `y`, `height`, `members`, `span_lo`,
#'   and `span_hi`. Carries a `"cut"` attribute recording the parameters used.
#'
#' @seealso [add_dendro_clades()] to draw the collapsed frontier.
#'
#' @examples
#' hc <- stats::hclust(stats::dist(matrix(rnorm(400), ncol = 4)))
#' d <- dendro_data(hc, hang = 0.03)
#' cut <- dendro_cut(d, max_leaves = 20)
#' nrow(cut$clades)
#'
#' @export
dendro_cut <- function(
    dendro,
    max_leaves = Inf,
    priority = c("members", "height", "hybrid"),
    min_clade = 1L
) {
  hc <- attr(dendro, "hclust")
  if (is.null(hc)) {
    stop(
      "dendro_cut() requires a dendro_data() result with an 'hclust' attribute.",
      call. = FALSE
    )
  }

  max_leaves <- .validate_max_leaves(max_leaves)
  min_clade <- .validate_min_clade(min_clade)

  inputs <- .normalize_hclust_inputs(hc)
  n_leaf <- nrow(inputs$merge) + 1L

  # Nothing to do: the caller's budget already covers every leaf.
  if (max_leaves >= n_leaf) {
    return(dendro)
  }

  tbl <- node_table(
    merge = inputs$merge,
    height = inputs$height,
    order = inputs$order
  )

  score <- .resolve_cut_priority(priority, tbl)
  peeled <- .peel_frontier(tbl, score, max_leaves, min_clade)

  # `labels` rows are in display order: row p holds leaf order[p] at x = p.
  # Invert that once so a merge's leaf child can be resolved by hclust index.
  leaf_row <- integer(n_leaf)
  leaf_row[inputs$order] <- seq_len(n_leaf)

  out <- dendro
  out$segments <- .cut_segments(dendro, tbl, peeled$expanded, leaf_row)
  out$clades <- .cut_clades(tbl, peeled$frontier, dendro, leaf_row)

  attr(out, "hclust") <- hc
  attr(out, "cut") <- list(
    max_leaves = max_leaves,
    priority = if (is.function(priority)) "custom" else match.arg(priority),
    min_clade = min_clade,
    expanded = length(peeled$expanded),
    clades = nrow(out$clades)
  )
  out
}

.validate_max_leaves <- function(max_leaves) {
  if (!is.numeric(max_leaves) || length(max_leaves) != 1L || is.na(max_leaves)) {
    stop("max_leaves must be a single number.", call. = FALSE)
  }
  if (max_leaves < 1) {
    stop("max_leaves must be at least 1.", call. = FALSE)
  }
  max_leaves
}

.validate_min_clade <- function(min_clade) {
  if (!is.numeric(min_clade) || length(min_clade) != 1L || is.na(min_clade) ||
      min_clade < 1) {
    stop("min_clade must be a single number of at least 1.", call. = FALSE)
  }
  as.integer(min_clade)
}

# A character choice maps to one of the built-in scores; a function is called
# with the node table's members and height and must return one score per merge.
.resolve_cut_priority <- function(priority, tbl) {
  if (is.function(priority)) {
    score <- priority(tbl$members, tbl$height)
    if (!is.numeric(score) || length(score) != nrow(tbl)) {
      stop(
        "A priority function must return one numeric score per merge.",
        call. = FALSE
      )
    }
    return(as.numeric(score))
  }

  switch(
    match.arg(priority, c("members", "height", "hybrid")),
    members = as.numeric(tbl$members),
    height = as.numeric(tbl$height),
    hybrid = as.numeric(tbl$height) * log1p(tbl$members)
  )
}

# Priority-queue peel. `frontier` holds merge-row indices (positive) and leaf
# indices (negative), matching hclust's own merge encoding, so a frontier entry
# can be handed straight back to the node table.
#
# Expanding a node only ever replaces it with its own two children, so the
# expanded set stays top-down closed no matter how `score` is ordered.
.peel_frontier <- function(tbl, score, max_leaves, min_clade) {
  m <- nrow(tbl)
  frontier <- m                      # the root merge row
  expanded <- integer(0)

  while (length(frontier) < max_leaves) {
    internal <- frontier[frontier > 0L]
    if (!length(internal)) {
      break                          # frontier is all leaves; cannot refine
    }

    # Stop subdividing clades that are already small: past this size the extra
    # detail is not legible anyway, and the budget buys more elsewhere.
    #
    # Note this gates on the clade's own size, not its children's. Gating on
    # children would deadlock immediately on a real tree: hierarchical
    # clusterings routinely attach a single outlier leaf at a high merge, so
    # the root itself has a one-member child and nothing would ever expand.
    if (min_clade > 1L) {
      internal <- internal[tbl$members[internal] >= min_clade]
      if (!length(internal)) {
        break
      }
    }

    pick <- internal[which.max(score[internal])]
    expanded <- c(expanded, pick)
    frontier <- c(
      frontier[frontier != pick],
      tbl$left[pick],
      tbl$right[pick]
    )
  }

  list(expanded = expanded, frontier = frontier)
}

# Segments of the reduced tree: the classic bracket for every expanded node --
# up from the left child, across at the merge height, down to the right child.
# A child that is itself collapsed terminates at its own merge height; a leaf
# child terminates at the leaf y the original geometry gave it, so `hang` is
# honoured without recomputation.
.cut_segments <- function(dendro, tbl, expanded, leaf_row) {
  if (!length(expanded)) {
    return(dendro$segments[0L, , drop = FALSE])
  }

  left <- tbl$left[expanded]
  right <- tbl$right[expanded]
  h <- tbl$height[expanded]

  lx <- .child_position(left, tbl, dendro, leaf_row)
  rx <- .child_position(right, tbl, dendro, leaf_row)
  ly <- .child_height(left, tbl, dendro, leaf_row)
  ry <- .child_height(right, tbl, dendro, leaf_row)

  # Three segments per bracket, matching the shape the full geometry emits.
  data.frame(
    x = c(lx, lx, rx),
    y = c(ly, h, h),
    xend = c(lx, rx, rx),
    yend = c(h, h, ry),
    stringsAsFactors = FALSE
  )
}

# Display x of a merge child: internal children sit at their node x, leaf
# children at their display position in the labels table.
.child_position <- function(k, tbl, dendro, leaf_row) {
  out <- numeric(length(k))
  internal <- k > 0L
  out[internal] <- tbl$x[k[internal]]
  if (any(!internal)) {
    out[!internal] <- dendro$labels$x[leaf_row[-k[!internal]]]
  }
  out
}

# Display y of a merge child: internal children sit at their own merge height,
# leaf children at whatever y the original geometry assigned (respecting hang).
.child_height <- function(k, tbl, dendro, leaf_row) {
  out <- numeric(length(k))
  internal <- k > 0L
  out[internal] <- tbl$height[k[internal]]
  if (any(!internal)) {
    out[!internal] <- dendro$labels$y[leaf_row[-k[!internal]]]
  }
  out
}

# The collapsed frontier, in display order. Single-leaf clades are retained so
# the frontier always accounts for every leaf; `members == 1` lets a renderer
# draw them differently from real aggregates.
.cut_clades <- function(tbl, frontier, dendro, leaf_row) {
  internal <- frontier[frontier > 0L]
  leaf_idx <- -frontier[frontier < 0L]
  rows <- leaf_row[leaf_idx]

  out <- data.frame(
    x = c(tbl$x[internal], dendro$labels$x[rows]),
    y = c(tbl$height[internal], dendro$labels$y[rows]),
    height = c(tbl$height[internal], dendro$labels$y[rows]),
    members = as.integer(c(tbl$members[internal], rep.int(1L, length(rows)))),
    span_lo = as.integer(c(tbl$span_lo[internal], dendro$labels$x[rows])),
    span_hi = as.integer(c(tbl$span_hi[internal], dendro$labels$x[rows])),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$span_lo), , drop = FALSE]
  rownames(out) <- NULL
  out
}
