.comb_hclust <- function(n = 8L) {
  stopifnot(n >= 2L)
  merge <- matrix(0L, nrow = n - 1L, ncol = 2L)
  merge[1L, ] <- c(-1L, -2L)
  if (n > 2L) {
    for (row in 2:(n - 1L)) {
      merge[row, ] <- c(row - 1L, -(row + 1L))
    }
  }
  structure(
    list(
      merge = merge,
      height = as.numeric(seq_len(n - 1L)),
      order = seq_len(n),
      labels = paste0("L", seq_len(n)),
      method = "complete",
      call = quote(.comb_hclust())
    ),
    class = "hclust"
  )
}

.balanced_hclust <- function() {
  structure(
    list(
      merge = matrix(
        c(
          -1L, -2L,
          -3L, -4L,
          -5L, -6L,
          -7L, -8L,
          1L, 2L,
          3L, 4L,
          5L, 6L
        ),
        ncol = 2L,
        byrow = TRUE
      ),
      height = c(1, 1, 1, 1, 2, 2, 3),
      order = seq_len(8L),
      labels = paste0("L", seq_len(8L)),
      method = "complete",
      call = quote(.balanced_hclust())
    ),
    class = "hclust"
  )
}
