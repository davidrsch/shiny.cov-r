# Regression tests for the manifest-snapshots.json read path:
# manifest_read_path()/read_manifest_from_path() in R/utils.R, called from
# both load_manifest() (R/report.R) and merge_ui_coverage() (R/collect.R).
#
# manifest-snapshots.json is a plain JSON array of raw, unmerged manifest
# snapshots -- written by an adapter that accumulates one snapshot per test
# worker instead of merging at write time (see merge_manifest_snapshots()'s
# docstring in R/utils.R). These tests confirm the read-time merge produces
# the same shape load_manifest()/merge_ui_coverage() already know how to
# consume, and that a plain manifest.json (no snapshots file present) is
# read exactly as before.

#' Write a manifest-snapshots.json array into a fresh temp app dir.
#' @param snapshots A list of manifest-shaped lists (the raw, unmerged
#'   per-worker snapshots).
#' @return Path to the temp app dir (caller is responsible for cleanup).
write_snapshots_fixture <- function(snapshots) {
  app_dir <- tempfile("shiny.cov-manifest-snapshots-")
  dir.create(app_dir, recursive = TRUE)
  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)
  jsonlite::write_json(
    snapshots,
    file.path(shinycov_dir, "manifest-snapshots.json"),
    auto_unbox = TRUE
  )
  app_dir
}

test_that("manifest_read_path() prefers manifest-snapshots.json when both files exist", {
  app_dir <- tempfile("shiny.cov-manifest-both-")
  dir.create(file.path(app_dir, ".shiny.cov"), recursive = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE))

  writeLines('{"inputs": [], "outputs": []}', file.path(app_dir, ".shiny.cov", "manifest.json"))
  writeLines("[]", file.path(app_dir, ".shiny.cov", "manifest-snapshots.json"))

  expect_identical(
    manifest_read_path(app_dir),
    file.path(app_dir, ".shiny.cov", "manifest-snapshots.json")
  )
})

test_that("manifest_read_path() falls back to manifest.json when no snapshots file exists", {
  app_dir <- tempfile("shiny.cov-manifest-plain-")
  dir.create(file.path(app_dir, ".shiny.cov"), recursive = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE))

  writeLines('{"inputs": [], "outputs": []}', file.path(app_dir, ".shiny.cov", "manifest.json"))

  expect_identical(
    manifest_read_path(app_dir),
    file.path(app_dir, ".shiny.cov", "manifest.json")
  )
})

test_that("load_manifest() merges a manifest-snapshots.json array via the N-way reduce", {
  snapshots <- list(
    list(
      inputs = list(list(id = "picker_1", type = "shiny.selectInput", label = "")),
      outputs = list(), tabs = list("Main"), conditional = list(), modules = list()
    ),
    list(
      inputs = list(list(id = "picker_1", type = "shinyWidgets.pickerInput", label = "Picker 1")),
      outputs = list(), tabs = list("Settings"), conditional = list(), modules = list()
    )
  )
  app_dir <- write_snapshots_fixture(snapshots)
  on.exit(cleanup_temp_app(app_dir))

  m <- load_manifest(app_dir)

  picker <- Filter(function(x) x$id == "picker_1", m$inputs)[[1]]
  expect_equal(picker$type, "shinyWidgets.pickerInput")
  expect_equal(picker$label, "Picker 1")
  expect_setequal(unlist(m$tabs), c("Main", "Settings"))
})

test_that("load_manifest() falls back to default_manifest() for an empty manifest-snapshots.json array", {
  app_dir <- write_snapshots_fixture(list())
  on.exit(cleanup_temp_app(app_dir))

  m <- load_manifest(app_dir)

  expect_equal(m$inputs, list())
  expect_equal(m$outputs, list())
})

test_that("merge_ui_coverage() merges a manifest-snapshots.json array the same way it merges manifest.json", {
  skip_if_no_covr()

  app_dir <- create_temp_app("single-line-widget-app")
  on.exit(cleanup_temp_app(app_dir))

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  eval_instrumented(inst, env = new.env())
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", class(cov))

  # Two raw, unmerged worker snapshots -- both see "go", neither pre-merged.
  # This is the exact shape a Playwright-style per-worker adapter dumps into
  # manifest-snapshots.json.
  snapshots <- list(
    list(inputs = list(list(id = "go", type = "actionButton")), outputs = list()),
    list(inputs = list(list(id = "go", type = "actionButton")), outputs = list())
  )
  jsonlite::write_json(snapshots, file.path(shinycov_dir, "manifest-snapshots.json"), auto_unbox = TRUE)
  jsonlite::write_json(
    list(list(selector = "#go", action = "click")),
    file.path(shinycov_dir, "interactions.json"),
    auto_unbox = TRUE
  )

  merged <- merge_ui_coverage(cov, app_dir)

  ui_key <- grep("^ui:go:", names(merged), value = TRUE)
  expect_gt(length(ui_key), 0)
  expect_equal(merged[[ui_key[[1]]]]$value, 1)
})

# ---- regression: existing adapters (manifest.json only) are unaffected ----

test_that("load_manifest() reads manifest.json unchanged when no manifest-snapshots.json is present", {
  app_dir <- tempfile("shiny.cov-manifest-regression-")
  dir.create(file.path(app_dir, ".shiny.cov"), recursive = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE))

  manifest <- list(
    inputs = list(list(id = "n", type = "shiny.numericInput", label = "Number")),
    outputs = list(),
    tabs = list("Main"),
    conditional = list(),
    modules = list()
  )
  jsonlite::write_json(manifest, file.path(app_dir, ".shiny.cov", "manifest.json"), auto_unbox = TRUE)

  m <- load_manifest(app_dir)

  expect_equal(m$inputs[[1]]$id, "n")
  expect_equal(m$inputs[[1]]$type, "shiny.numericInput")
  expect_equal(m$inputs[[1]]$label, "Number")
  expect_equal(unlist(m$tabs), "Main")
})

test_that("merge_ui_coverage() reads manifest.json unchanged when no manifest-snapshots.json is present", {
  skip_if_no_covr()

  manifest <- list(inputs = list(list(id = "go", type = "actionButton")), outputs = list())
  app_dir <- create_temp_app("single-line-widget-app")
  on.exit(cleanup_temp_app(app_dir))

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)
  jsonlite::write_json(manifest, file.path(shinycov_dir, "manifest.json"), auto_unbox = TRUE)
  jsonlite::write_json(
    list(list(selector = "#go", action = "click")),
    file.path(shinycov_dir, "interactions.json"),
    auto_unbox = TRUE
  )

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  eval_instrumented(inst, env = new.env())
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", class(cov))

  merged <- merge_ui_coverage(cov, app_dir)

  ui_key <- grep("^ui:go:", names(merged), value = TRUE)
  expect_gt(length(ui_key), 0)
  expect_equal(merged[[ui_key[[1]]]]$value, 1)
})
