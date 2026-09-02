# UI manifest tests — Phase 3 placeholders
# These tests will be expanded when the UI manifest feature is implemented.

test_that("ui_report() returns a valid (empty) structure", {
  skip_if_no_covr()

  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  outfile <- file.path(app_dir, ".shiny.cov", "coverage.rds")

  app_path <- file.path(app_dir, "app.R")
  inst <- instrument_file(app_path)
  saveRDS(as.list(inst$counters), outfile)

  cov <- collect(app_dir)
  ui <- ui_report(cov)

  expect_type(ui, "list")
  expect_true("inputs" %in% names(ui))
  expect_true("outputs" %in% names(ui))
})

test_that("report() accepts a valid coverage object", {
  skip_if_no_covr()

  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  outfile <- file.path(app_dir, ".shiny.cov", "coverage.rds")

  app_path <- file.path(app_dir, "app.R")
  inst <- instrument_file(app_path)
  saveRDS(as.list(inst$counters), outfile)

  cov <- collect(app_dir)
  expect_s3_class(cov, "coverage")
  # report() is tested separately via integration tests
})

test_that("report() errors on non-coverage input", {
  expect_error(
    report(list(1, 2, 3)),
    "coverage object"
  )
})

# UI discovery lives in inst/js/discover-bindings.js, run against a real,
# live browser via shiny.cov::AppDriver$discover_manifest() (shinytest2)
# or cy.shinyCovDiscoverManifest() (Cypress) -- see test-e2e.R for the
# browser-based regression test covering this (including the actionButton
# and custom-widget-binding cases a CSS-class approach can't handle).
# There is no server-side R function to unit test here.
