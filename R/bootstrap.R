#' Write the shiny.cov bootstrap script
#'
#' Copies the template from `inst/bootstrap/bootstrap.R` to `path`.
#' The bootstrap is sourced by the app wrapper created in [setup()].
#'
#' @param path Path where the bootstrap script should be written.
#' @param box_modules Ignored (kept for backward compatibility).
#' @return Invisibly returns `path`.
#' @keywords internal
write_bootstrap <- function(path, box_modules = TRUE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  template <- bootstrap_template_path()
  if (!is.null(template) && file.exists(template)) {
    file.copy(template, path, overwrite = TRUE)
  } else {
    stop("Bootstrap template not found at ", template)
  }
  invisible(path)
}

#' Path to the installed bootstrap template
#' @return Character path or `NULL`.
#' @keywords internal
bootstrap_template_path <- function() {
  system.file(
    "bootstrap",
    "bootstrap.R",
    package = "shiny.cov",
    mustWork = FALSE
  )
}
