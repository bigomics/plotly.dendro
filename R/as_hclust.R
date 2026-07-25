#' Coerce Input To `hclust`
#'
#' Passes `hclust` input through unchanged; runs `distfun`/`linkagefun` on
#' matrix-like or `dist` input to produce one.
#'
#' @param x A numeric matrix/data frame, `dist`, or `hclust` object.
#' @param distfun Distance function used when `x` is matrix-like.
#' @param linkagefun Linkage function used when `x` is matrix-like or `dist`.
#'
#' @return An `hclust` object.
as_hclust <- function(
    x,
    distfun = stats::dist,
    linkagefun = function(d) stats::hclust(d, method = "complete")
) {
  if (inherits(x, "hclust")) {
    return(x)
  }

  if (inherits(x, "dist")) {
    hc <- linkagefun(x)
    if (!inherits(hc, "hclust")) {
      stop("linkagefun must return an hclust object.", call. = FALSE)
    }
    return(hc)
  }

  if (is.matrix(x) || is.data.frame(x)) {
    d <- distfun(x)
    if (!inherits(d, "dist")) {
      stop("distfun must return a dist object.", call. = FALSE)
    }
    hc <- linkagefun(d)
    if (!inherits(hc, "hclust")) {
      stop("linkagefun must return an hclust object.", call. = FALSE)
    }
    return(hc)
  }

  stop("Unsupported input type for as_hclust().", call. = FALSE)
}
