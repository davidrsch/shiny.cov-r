# Regression tests for R/report.R:
#
# - apply_module_boundaries() attributes each manifest input/output to
#   the real module boundary it belongs to (from ground-truth modules.rds,
#   written by the shiny::moduleServer() hook in bootstrap.R), via plain
#   "-"-prefix string matching against that boundary list. A top-level,
#   non-module id that merely *happens* to share a real module's prefix
#   (e.g. a plain input literally named "sidebar-toggle" when the app also
#   has an unrelated real module called "sidebar") is byte-for-byte
#   indistinguishable from a genuine child of that module using only this
#   data -- see the long comment inside apply_module_boundaries() for the
#   full reasoning. These tests lock in the corroboration requirement
#   (don't attribute a *prefix* match unless at least one other id in the
#   same manifest corroborates that boundary) without regressing the
#   genuinely-nested case, which stays exactly as precise as a plain
#   longest-prefix match.

#' Write a `.shiny.cov/manifest.json`-shaped list and (optionally) a
#' `modules.rds` ground-truth boundary list into a fresh temp app dir, the
#' same on-disk shape `load_manifest()`/`apply_module_boundaries()` read.
#'
#' @param manifest A list with $inputs/$outputs (plain, undecorated --
#'   i.e. without a `module` field, matching what discover-bindings.js
#'   actually writes; `module` is populated by apply_module_boundaries()).
#' @param boundaries Character vector of real module boundary ids, or
#'   `NULL` to skip writing modules.rds entirely.
#' @return Path to the temp app dir (caller is responsible for cleanup).
write_module_fixture <- function(manifest, boundaries = NULL) {
  app_dir <- tempfile("shiny.cov-module-fixture-")
  dir.create(app_dir, recursive = TRUE)
  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)

  jsonlite::write_json(manifest, file.path(shinycov_dir, "manifest.json"), auto_unbox = TRUE)
  if (!is.null(boundaries)) {
    saveRDS(boundaries, file.path(shinycov_dir, "modules.rds"))
  }
  app_dir
}

test_that("a top-level id coinciding with a real module's prefix is not misattributed", {
  # "sidebar" is real ground truth (moduleServer("sidebar", ...) really ran
  # somewhere in this app) but, in *this* manifest, has no other element
  # under it -- exactly the case this iteration's corroboration check
  # catches: a lone id starting with "sidebar-" is exactly as consistent
  # with "coincidental top-level id" as with "real module with one child,"
  # so it's left unattributed rather than guessed.
  manifest <- list(
    inputs = list(list(id = "sidebar-toggle", type = "shiny.actionButtonInput")),
    outputs = list()
  )
  app_dir <- write_module_fixture(manifest, boundaries = "sidebar")
  on.exit(unlink(app_dir, recursive = TRUE))

  cov <- structure(list(), class = "coverage")
  ui <- ui_report(cov, app_dir = app_dir)

  entry <- Filter(function(x) x$id == "sidebar-toggle", ui$inputs)[[1]]
  expect_equal(entry$module, "")
})

test_that("genuinely nested module ids are still attributed correctly", {
  # "counter" has *two* real children in this manifest (bin_1 and total),
  # so the corroboration check is satisfied and attribution stays exactly
  # as precise as the pre-existing longest-match logic.
  manifest <- list(
    inputs = list(
      list(id = "counter-bin_1", type = "shiny.sliderInput"),
      list(id = "choice", type = "shiny.selectInput") # unrelated top-level id
    ),
    outputs = list(
      list(id = "counter-total", type = "shiny.textOutput")
    )
  )
  app_dir <- write_module_fixture(manifest, boundaries = "counter")
  on.exit(unlink(app_dir, recursive = TRUE))

  cov <- structure(list(), class = "coverage")
  ui <- ui_report(cov, app_dir = app_dir)

  bin1 <- Filter(function(x) x$id == "counter-bin_1", ui$inputs)[[1]]
  expect_equal(bin1$module, "counter")
  total <- Filter(function(x) x$id == "counter-total", ui$outputs)[[1]]
  expect_equal(total$module, "counter")
  choice <- Filter(function(x) x$id == "choice", ui$inputs)[[1]]
  expect_equal(choice$module, "")
})

test_that("an exact id == boundary match is trusted even without corroboration", {
  # identical(id, b) is a much narrower coincidence than a shared prefix
  # (the whole id, not just a prefix, would have to accidentally match a
  # real module's bare namespace) -- apply_module_boundaries() keeps
  # trusting it unconditionally, unlike prefix matches.
  manifest <- list(inputs = list(list(id = "sidebar", type = "shiny.actionButtonInput")), outputs = list())
  app_dir <- write_module_fixture(manifest, boundaries = "sidebar")
  on.exit(unlink(app_dir, recursive = TRUE))

  cov <- structure(list(), class = "coverage")
  ui <- ui_report(cov, app_dir = app_dir)

  entry <- Filter(function(x) x$id == "sidebar", ui$inputs)[[1]]
  expect_equal(entry$module, "sidebar")
})

test_that("apply_module_boundaries() is a no-op when modules.rds is absent", {
  manifest <- list(
    inputs = list(list(id = "sidebar-toggle", type = "shiny.actionButtonInput")),
    outputs = list()
  )
  app_dir <- write_module_fixture(manifest, boundaries = NULL)
  on.exit(unlink(app_dir, recursive = TRUE))

  cov <- structure(list(), class = "coverage")
  ui <- ui_report(cov, app_dir = app_dir)

  entry <- Filter(function(x) x$id == "sidebar-toggle", ui$inputs)[[1]]
  expect_equal(entry$module, "")
})

test_that("two elements sharing one id can still corroborate each other", {
  # has_corroboration() excludes an id from `other` *by position*, not by
  # value -- if it excluded by value instead, two genuinely distinct
  # elements (e.g. an input and an output) sharing the exact same id string
  # would exclude "every element named X" instead of "just this specific
  # element," leaving neither able to corroborate the other, even though
  # there are two real elements under the boundary.
  manifest <- list(
    inputs = list(list(id = "sidebar-toggle", type = "shiny.actionButtonInput")),
    outputs = list(list(id = "sidebar-toggle", type = "shiny.textOutput"))
  )
  app_dir <- write_module_fixture(manifest, boundaries = "sidebar")
  on.exit(unlink(app_dir, recursive = TRUE))

  cov <- structure(list(), class = "coverage")
  ui <- ui_report(cov, app_dir = app_dir)

  input_entry <- Filter(function(x) x$id == "sidebar-toggle", ui$inputs)[[1]]
  output_entry <- Filter(function(x) x$id == "sidebar-toggle", ui$outputs)[[1]]
  expect_equal(input_entry$module, "sidebar")
  expect_equal(output_entry$module, "sidebar")
})

test_that("a malformed interactions.json entry does not crash ui_report()/report()", {
  # A logged NULL selector/action serializes via jsonlite as an empty list
  # `{}`, not JSON null. R/collect.R's merge_ui_coverage() was already
  # hardened against this shape; build_ui_coverage() had the identical gap.
  manifest <- list(
    inputs = list(list(id = "go", type = "shiny.actionButtonInput")),
    outputs = list()
  )
  app_dir <- write_module_fixture(manifest, boundaries = NULL)
  on.exit(unlink(app_dir, recursive = TRUE))

  jsonlite::write_json(
    list(list(selector = "#go", action = list())),
    file.path(app_dir, ".shiny.cov", "interactions.json"),
    auto_unbox = TRUE
  )

  cov <- structure(list(), class = "coverage")
  expect_error(ui_report(cov, app_dir = app_dir), NA)

  ui <- ui_report(cov, app_dir = app_dir)
  entry <- Filter(function(x) x$id == "go", ui$inputs)[[1]]
  expect_false(entry$interacted) # malformed action never matches a real one
})
