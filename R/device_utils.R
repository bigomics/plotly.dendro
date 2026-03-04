.with_null_device <- function(expr) {
  opened <- FALSE
  if (grDevices::dev.cur() == 1L) {
    grDevices::pdf(file = NULL)
    opened <- TRUE
  }
  on.exit({
    if (opened && grDevices::dev.cur() > 1L) {
      grDevices::dev.off()
    }
  }, add = TRUE)

  force(expr)
}
