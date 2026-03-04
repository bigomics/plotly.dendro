# Shared test datasets — sourced automatically by testthat before all test files.

# tiny4: 4 observations, easy to verify by hand.
# Produces a tree with exactly 3 merges → 4*(4-1) = 12 segment rows.
tiny4 <- matrix(
  c(1, 2,
    3, 4,
    1, 4,
    2, 3),
  nrow = 4, byrow = TRUE,
  dimnames = list(c("A", "B", "C", "D"), c("x", "y"))
)

# medium: 10 USArrests states → 4*(10-1) = 36 segment rows.
medium <- USArrests[1:10, ]

# full: all 50 USArrests states — the canonical ggdendro reference.
# ggdendro::dendro_data(hclust(dist(full), "ave"))$segments has 196 rows.
full <- USArrests

# Shared linkage function used across all phases for reproducibility.
hclust_complete <- function(d) stats::hclust(d, method = "complete")
