# Internal helper utilities for shiny.cov

# ---- Path helpers ----

#' Determine the shiny.cov output directory for a given app directory
#'
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to the `.shiny.cov/` subdirectory.
#' @keywords internal
shinycov_output_dir <- function(app_dir) {
  file.path(app_dir, ".shiny.cov")
}

#' Full path to the coverage RDS file
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to `coverage.rds`.
#' @keywords internal
shinycov_output_file <- function(app_dir) {
  file.path(shinycov_output_dir(app_dir), "coverage.rds")
}

#' Full path to the bootstrap R script
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to `bootstrap.R`.
#' @keywords internal
shinycov_bootstrap_path <- function(app_dir) {
  file.path(shinycov_output_dir(app_dir), "bootstrap.R")
}

#' Full path to the id -> srcref attribution RDS file
#'
#' Written by the `htmltools::tag()` hook in `bootstrap.R` (see there for
#' why it's needed); read by `merge_ui_coverage()` in `R/collect.R` as a
#' fallback when literal text search can't locate an id (e.g. computed ids
#' built with `paste0()` in a loop).
#'
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to `id_srcrefs.rds`.
#' @keywords internal
shinycov_id_srcrefs_file <- function(app_dir) {
  file.path(shinycov_output_dir(app_dir), "id_srcrefs.rds")
}

#' Full path to the real module-boundary RDS file
#'
#' Written by the `shiny::moduleServer()` hook in `bootstrap.R`: the set of
#' id prefixes that are *actually* module namespace boundaries at runtime,
#' as ground truth for `manifest$modules` and each element's `module`
#' field (see `apply_module_boundaries()` in `R/report.R`).
#'
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to `modules.rds`.
#' @keywords internal
shinycov_modules_file <- function(app_dir) {
  file.path(shinycov_output_dir(app_dir), "modules.rds")
}

#' Full path to the manifest-snapshots file
#'
#' An alternate write-time shape to `manifest.json`: a plain JSON array of
#' raw, unmerged manifest snapshots (one per test worker), for an adapter
#' that can't coordinate a single merged write across parallel workers and
#' instead just accumulates each worker's snapshot independently. See
#' [read_manifest_from_path()] for the corresponding read-time merge.
#'
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to `manifest-snapshots.json`.
#' @keywords internal
shinycov_manifest_snapshots_file <- function(app_dir) {
  file.path(shinycov_output_dir(app_dir), "manifest-snapshots.json")
}

#' Resolve which manifest file to read for an app directory
#'
#' `manifest-snapshots.json` takes priority over `manifest.json` when both
#' are present. In practice an adapter writes one shape or the other, never
#' both, but preferring the snapshot file is the conservative choice if a
#' stale `manifest.json` from a previous run happens to still be on disk.
#'
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to whichever manifest file should be read.
#'   Existence is not checked here -- callers already do their own
#'   `file.exists()` check on the returned path.
#' @keywords internal
manifest_read_path <- function(app_dir) {
  snapshots_path <- shinycov_manifest_snapshots_file(app_dir)
  if (file.exists(snapshots_path)) {
    snapshots_path
  } else {
    file.path(shinycov_output_dir(app_dir), "manifest.json")
  }
}

#' Read a manifest file, merging it first if it's the snapshot-array format
#'
#' A `manifest.json` parses directly into a manifest list, same as always.
#' A `manifest-snapshots.json` parses into a JSON array of raw, unmerged
#' manifest snapshots -- one per test worker -- which this reduces through
#' `merge_manifest_snapshots()`'s existing pairwise merge before returning,
#' so callers see one manifest list regardless of which shape was on disk.
#' Reducing left to right over the array gives the same result as if the
#' snapshots had arrived one at a time to a single accumulator, and is
#' order-independent for the realistic case the merge algorithm's
#' "more-specific-type-wins" rule is designed for: once an id's specific
#' type is merged in, it can never be displaced by a later base/generic
#' snapshot, so it doesn't matter which snapshot in the array happened to
#' carry it. (It is not order-independent in the degenerate case of two
#' snapshots disagreeing on *which* specific type an id has -- whichever
#' one is merged in first wins. That case doesn't arise from how a single
#' widget's DOM degrades over time, the scenario this format exists for.)
#'
#' An empty snapshot array reduces to `NULL` (`Reduce()`'s behavior on an
#' empty list with no `init` supplied) -- callers already treat a `NULL`
#' manifest as "nothing to report" wherever they parse `manifest.json`
#' today, so this needs no special-casing here.
#'
#' @param path Path to a `manifest.json` or `manifest-snapshots.json` file,
#'   as returned by [manifest_read_path()].
#' @return The parsed manifest list. Propagates any `jsonlite::fromJSON()`
#'   parse error to the caller, same as parsing `manifest.json` directly
#'   always has.
#' @keywords internal
read_manifest_from_path <- function(path) {
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (identical(basename(path), "manifest-snapshots.json")) {
    Reduce(merge_manifest_snapshots, parsed)
  } else {
    parsed
  }
}

#' Path to the UI-discovery JS script
#'
#' Runs inside a live browser (via `AppDriver$get_js()`, Cypress's
#' `cy.window()`, or Playwright's `page.evaluate()`) to discover
#' inputs/outputs via Shiny's own `Shiny.inputBindings`/`Shiny.outputBindings`
#' registry -- see the script itself for why this generalizes to any widget
#' library.
#'
#' @return Character path, or `NULL` if not found.
#' @keywords internal
discover_bindings_js_path <- function() {
  system.file("js", "discover-bindings.js", package = "shiny.cov", mustWork = FALSE)
}

#' Read the UI-discovery JS script as a single string
#' @return Character scalar.
#' @keywords internal
discover_bindings_js <- function() {
  path <- discover_bindings_js_path()
  if (!nzchar(path) || !file.exists(path)) {
    stop("shiny.cov: discover-bindings.js not found -- is shiny.cov installed correctly?")
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

#' Merge two UI manifest snapshots taken at different points in a test
#'
#' `discover-bindings.js` (and its vendored copies used by the Cypress and
#' Playwright adapters) already dedupe *within* a single snapshot when one
#' element matches more than one registered binding, preferring the more
#' specific, non-"shiny."-prefixed
#' name. But some widget libraries mutate their own element's class list
#' after interaction -- `shinyWidgets::pickerInput()` (bootstrap-select) is
#' an example: its wrapping
#' `.selectpicker` class is present at initial render but gets stripped by
#' the library's own JS once `set_inputs()` changes the value, so a
#' snapshot taken only *after* interactions can see just the generic
#' `shiny.selectInput` match that's left once the more specific one no
#' longer matches -- not because discovery is wrong, but because the DOM
#' genuinely looks different by then. Taking a snapshot at load time too
#' (before anything can degrade) and merging across snapshots the same way
#' -- prefer the more specific type wherever it was seen at *any* point --
#' fixes this without depending on interaction timing.
#'
#' Coverage merging/interaction tracking are id-based, not type-based, so
#' this mislabeling never affected correctness -- only the human-readable
#' `type` shown in manifests and `ui_report()`. Still worth fixing since
#' it's real, not hypothetical.
#'
#' @param old Previous manifest list (or `NULL`/empty if none yet).
#' @param new Newly-discovered manifest list.
#' @return Merged manifest list.
#' @keywords internal
merge_manifest_snapshots <- function(old, new) {
  if (is.null(old) || length(old) == 0) return(new)

  is_more_specific <- function(candidate_type, current_type) {
    current_is_base <- is.null(current_type) || !nzchar(current_type) ||
      startsWith(current_type, "shiny.")
    candidate_is_base <- is.null(candidate_type) || !nzchar(candidate_type) ||
      startsWith(candidate_type, "shiny.")
    current_is_base && !candidate_is_base
  }

  merge_elements <- function(old_els, new_els) {
    by_id <- list()
    for (el in old_els) {
      if (!is.null(el$id) && nzchar(el$id)) by_id[[el$id]] <- el
    }
    for (el in new_els) {
      id <- el$id
      if (is.null(id) || !nzchar(id)) next
      existing <- by_id[[id]]
      if (is.null(existing)) {
        by_id[[id]] <- el
        next
      }
      merged <- existing
      if (is_more_specific(el$type, existing$type)) merged$type <- el$type
      if ((is.null(merged$label) || !nzchar(merged$label %||% "")) &&
          !is.null(el$label) && nzchar(el$label)) {
        merged$label <- el$label
      }
      by_id[[id]] <- merged
    }
    unname(by_id)
  }

  list(
    inputs      = merge_elements(old$inputs, new$inputs),
    outputs     = merge_elements(old$outputs, new$outputs),
    tabs        = union(unlist(old$tabs), unlist(new$tabs)),
    conditional = union(unlist(old$conditional), unlist(new$conditional)),
    modules     = union(unlist(old$modules), unlist(new$modules))
  )
}

#' Locate the .Rprofile for an app directory
#'
#' Always `<app_dir>/.Rprofile` -- deliberately does *not* fall back to
#' `R_PROFILE_USER`. That env var is a session-global setting unrelated to
#' `app_dir`: honoring it would write the snippet to whatever arbitrary
#' file the *calling* process happens to have set (this occurs under R CMD
#' check's own test harness), not to the app directory the caller passed in.
#'
#' @param app_dir Path to the Shiny app directory.
#' @return Character path to the `.Rprofile` file.
#' @keywords internal
app_rprofile_path <- function(app_dir) {
  file.path(app_dir, ".Rprofile")
}

# ---- Backup helpers ----

#' Backup suffix for .Rprofile
#' @keywords internal
backup_suffix <- ".shinycov_backup"

#' Path to the .Rprofile backup file
#' @param app_dir Path to the Shiny app directory.
#' @return Character path.
#' @keywords internal
rprofile_backup_path <- function(app_dir) {
  rp <- app_rprofile_path(app_dir)
  paste0(rp, backup_suffix)
}

# ---- Null-coalesce ----

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Interaction-log action vocabulary ----
#
# Interaction log entries come from three different loggers that don't share
# code: shinytest2's AppDriver wrapper (R/shinytest2.R) logs actions named
# set_inputs/click/upload_file/get_value/get_text/get_html, the Cypress
# support commands (shiny.cov-cypress/src/support.js) log
# click/type/select/check/uncheck/get_text/get_html, and the Playwright
# fixtures (shiny.cov-playwright/src/fixtures.js) log the literal Playwright
# Locator/Page method names its Proxy-wrapping intercepts --
# click/dblclick/tap/check/uncheck/fill/type/press/pressSequentially/
# selectOption/setInputFiles/dragTo/clear/selectText/focus/hover for inputs,
# textContent/innerText/innerHTML/getAttribute/inputValue for outputs. Both
# merge_ui_coverage() (R/collect.R) and build_ui_coverage() (R/report.R)
# need to agree on which of these count as an "input interaction" vs an
# "output verification" -- centralize the list here instead of hardcoding
# three different subsets.

#' Action names that count as interacting with an input
#' @return Character vector.
#' @keywords internal
shinycov_input_actions <- function() {
  c(
    "set_inputs", "click", "upload_file", "get_value", # shinytest2
    "type", "select", "check", "uncheck", # Cypress (click/type/check/uncheck overlap with Playwright below)
    "dblclick", "tap", "fill", "press", "pressSequentially", # Playwright
    "selectOption", "setInputFiles", "dragTo", "clear", "selectText", "focus", "hover"
  )
}

#' Action names that count as verifying an output
#' @return Character vector.
#' @keywords internal
shinycov_output_actions <- function() {
  c(
    "get_text", "get_html", # shinytest2 / Cypress
    "textContent", "innerText", "innerHTML", "getAttribute", "inputValue" # Playwright
  )
}
