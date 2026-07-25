## Zoom-driven dendrogram on the 4,924-leaf WGCNA reference tree.
##
## Produces a self-contained HTML file: no Shiny, no server. Zooming the leaf
## axis re-cuts the tree in the browser, so detail resolves rather than merely
## magnifying.
##
## Run from the plotly.dendro repository root:
##   Rscript --vanilla inst/examples/dynamic_dendro.R [out.html]

suppressPackageStartupMessages({
  library(plotly)
  library(htmlwidgets)
})

if (requireNamespace("pkgload", quietly = TRUE)) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  library(plotly.dendro)
}

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "dynamic_dendro.html"

ref <- ".agents_context/perf_optim/reference/aml_18celllines_multiomics.input.rds"
if (file.exists(ref)) {
  input <- readRDS(ref)
  hc <- input$hclust
  leaf_color <- input$module_colors[hc$order]
} else {
  # Fall back to synthetic data so the example runs outside the dev checkout.
  set.seed(1)
  hc <- stats::hclust(stats::dist(matrix(stats::rnorm(8000), ncol = 4)))
  leaf_color <- sample(
    c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"),
    length(hc$order), replace = TRUE
  )
}

d <- dendro_data(hc, hang = 0.03)

p <- plot_ly() |>
  add_dendro_dynamic(
    d,
    max_leaves = 300,
    budget_mode = "pixels",
    min_leaf_px = 3,
    leaf_color = leaf_color,
    show_labels = TRUE
  ) |>
  add_dendro_layout(d, show_labels = FALSE) |>
  layout(
    title = list(
      text = sprintf(
        "Dynamic dendrogram - %s leaves, re-cut on zoom",
        format(length(hc$order), big.mark = ",")
      ),
      x = 0.02, xanchor = "left"
    ),
    margin = list(t = 60, l = 60, r = 20, b = 40)
  )

saveWidget(p, file = normalizePath(out, mustWork = FALSE), selfcontained = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1e6, 2), " MB)")
