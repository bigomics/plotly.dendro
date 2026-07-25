# JavaScript Runtime Integration
#
# Attaches the plotly-dendro JS runtime and its configuration payload to a
# plotly object. Handles asset location in both installed and development
# contexts, and bootstraps with a retry so the hook survives script-load
# ordering.

NULL

#' Attach The Dynamic Dendrogram Runtime
#'
#' @param p A plotly object.
#' @param cfg Configuration list from `.build_dynamic_config()`.
#' @return The plotly object with the dependency and render hook attached.
#' @keywords internal
.attach_dendro_runtime <- function(p, cfg) {
  asset_dir <- .locate_dendro_asset_dir()

  dep <- htmltools::htmlDependency(
    name = "plotly-dendro",
    version = as.character(utils::packageVersion("plotly.dendro")),
    src = c(file = asset_dir),
    script = c(
      "dendro-peel.js",
      "dendro-geometry.js",
      "plotly-dendro.js"
    ),
    all_files = FALSE
  )

  # The dependency may not have executed by the time the render hook runs, so
  # poll briefly rather than assuming load order.
  init_js <- htmlwidgets::JS(
    "function(el, x, data) {
      var attempts = 20;
      function boot() {
        if (window.plotlyDendro && typeof window.plotlyDendro.init === 'function') {
          window.plotlyDendro.init(el, data);
          return;
        }
        attempts -= 1;
        if (attempts >= 0) {
          setTimeout(boot, 50);
        } else {
          console.warn('plotly-dendro runtime not loaded');
        }
      }
      boot();
    }"
  )

  # Deduplicate by name so repeated calls do not grow the dependency list.
  deps <- p$dependencies
  if (is.null(deps)) {
    deps <- list()
  }
  deps <- Filter(function(d) !identical(d$name, dep$name), deps)
  p$dependencies <- c(deps, list(dep))
  p <- htmltools::attachDependencies(p, dep, append = TRUE)

  htmlwidgets::onRender(p, init_js, data = cfg)
}

#' Locate The plotly-dendro JS Assets
#'
#' Supports installed-package usage and development workflows loaded via
#' `pkgload::load_all()`, where `system.file(package = ...)` can be empty.
#'
#' @return Absolute path to the asset directory.
#' @keywords internal
.locate_dendro_asset_dir <- function() {
  installed <- system.file("htmlwidgets/plotly-dendro", package = "plotly.dendro")
  if (nzchar(installed) && dir.exists(installed)) {
    return(installed)
  }

  if (requireNamespace("pkgload", quietly = TRUE)) {
    from_pkgload <- tryCatch(
      pkgload::pkg_path("inst", "htmlwidgets", "plotly-dendro"),
      error = function(...) ""
    )
    if (nzchar(from_pkgload) && dir.exists(from_pkgload)) {
      return(from_pkgload)
    }
  }

  local_inst <- normalizePath(
    file.path("inst", "htmlwidgets", "plotly-dendro"),
    mustWork = FALSE
  )
  if (dir.exists(local_inst)) {
    return(local_inst)
  }

  stop(
    "Could not locate plotly-dendro JS assets. Expected inst/htmlwidgets/plotly-dendro.",
    call. = FALSE
  )
}
