# Compatibility shim for testthat versions where expect_s3_class()
# does not accept a label argument.
expect_s3_class <- function(object, class, ..., info = NULL, label = NULL) {
  dots <- list(...)
  args <- c(list(object = object, class = class), dots)

  fm <- names(formals(testthat::expect_s3_class))
  if ("info" %in% fm && !is.null(info)) {
    args$info <- info
  }
  if ("label" %in% fm && !is.null(label)) {
    args$label <- label
  }

  do.call(testthat::expect_s3_class, args)
}
