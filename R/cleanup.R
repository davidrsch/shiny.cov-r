#' Clean up shiny.cov temporary files and restore originals
#'
#' Reverses the effects of [setup()]:
#'
#' 1. Restores original `app.R` from `.shinycov_backup`.
#' 2. Restores `.Rprofile` from backup or removes the snippet.
#' 3. Removes the `.shiny.cov/` output directory.
#' 4. Unsets environment variables.
#'
#' @param app_dir Path to the Shiny app directory. Defaults to `"."`.
#' @param remove_output If `TRUE`, remove the `.shiny.cov/` directory.
#' @param remove_rprofile_snippet If `TRUE`, restore `.Rprofile`.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shiny.cov::cleanup("my-shiny-app")
#' }
cleanup <- function(
  app_dir = ".",
  remove_output = TRUE,
  remove_rprofile_snippet = TRUE
) {
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = FALSE)
  if (is.na(app_dir) || !dir.exists(app_dir)) {
    warning("App directory not found: ", app_dir)
    return(invisible(FALSE))
  }

  # ---- 1. Restore app.R from wrapper backup ----
  restore_app_wrappers(app_dir)

  # ---- 2. Restore .Rprofile ----
  if (remove_rprofile_snippet) {
    restore_rprofile(app_dir)
  }

  # ---- 3. Remove output directory ----
  if (remove_output) {
    output_dir <- shinycov_output_dir(app_dir)
    if (dir.exists(output_dir)) {
      unlink(output_dir, recursive = TRUE, force = TRUE)
      message("Removed ", output_dir)
    }
  }

  # ---- 4. Unset env vars ----
  Sys.unsetenv("SHINYCOV_OUTPUT")
  Sys.unsetenv("R_COVR")
  Sys.unsetenv("R_PROFILE_USER")

  .shinycov_env$app_dir <- NULL

  message("shiny.cov cleanup complete.")
  invisible(TRUE)
}

# ---- Helpers ----

restore_app_wrappers <- function(app_dir) {
  backups <- list.files(
    app_dir,
    pattern = "\\.R\\.shinycov_backup$",
    full.names = TRUE
  )
  for (backup in backups) {
    original <- sub("\\.shinycov_backup$", "", backup)
    copied <- file.copy(backup, original, overwrite = TRUE)
    if (!copied) {
      stop(
        "Failed to restore ", basename(original), " from its backup at ",
        backup, " -- the backup was left in place so it can be restored ",
        "manually."
      )
    }
    file.remove(backup)
    message("Restored original ", basename(original))
  }

  # Remove wrapper if it exists but no backup (ui.R+server.R case)
  app_r <- file.path(app_dir, "app.R")
  if (file.exists(app_r)) {
    first_line <- tryCatch(
      readLines(app_r, n = 1, warn = FALSE),
      error = function(e) character(0)
    )
    # readLines(n = 1) on a genuinely 0-byte app.R returns character(0),
    # not "" -- guard that explicitly so the condition below is always a
    # real TRUE/FALSE (grepl() on character(0) returns logical(0), which
    # would otherwise propagate into `if (...)` as NA).
    if (
      length(first_line) > 0 &&
        grepl("shiny.cov app wrapper", first_line, fixed = TRUE) &&
        length(backups) == 0
    ) {
      file.remove(app_r)
      message("Removed app.R wrapper")
    }
  }
}

restore_rprofile <- function(app_dir) {
  rprofile_path <- app_rprofile_path(app_dir)
  backup_path <- rprofile_backup_path(app_dir)

  if (file.exists(backup_path)) {
    file.copy(backup_path, rprofile_path, overwrite = TRUE)
    file.remove(backup_path)
    message("Restored original .Rprofile from backup")
  } else if (file.exists(rprofile_path)) {
    content <- readLines(rprofile_path, warn = FALSE)
    start <- grep(
      "<<< shiny.cov coverage instrumentation >>>",
      content,
      fixed = TRUE
    )
    end <- grep("<<< end shiny.cov >>>", content, fixed = TRUE)
    if (length(start) > 0 && length(end) > 0) {
      content <- content[-(max(1, start[1] - 1):end[1])]
      # No backup exists at this point, which means setup() created this
      # .Rprofile from scratch (see inject_rprofile()'s "file didn't
      # exist" branch, which writeLines()s the snippet directly with no
      # backup taken -- there was nothing to back up). If stripping the
      # snippet leaves nothing behind (or only blank lines), the file's
      # *entire* content was the injected snippet, so the correct
      # restoration is to delete the file, matching the pre-setup()
      # state, instead of leaving a stray empty .Rprofile that didn't
      # exist before setup() ran.
      if (length(content) == 0 || !any(nzchar(trimws(content)))) {
        file.remove(rprofile_path)
        message("Removed .Rprofile created by shiny.cov (no backup existed)")
      } else {
        writeLines(content, rprofile_path)
        message("Removed shiny.cov snippet from .Rprofile")
      }
    }
  }
}
