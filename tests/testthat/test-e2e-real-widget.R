# Real third-party widget library regression test.
#
# Every other e2e fixture uses base Shiny widgets, plus one hand-registered
# *stand-in* custom binding (test-e2e.R's "weird"/"demo.customWidget") meant
# to represent what a library like shiny.fluent would look like. A stand-in
# is not the same as proof: this test uses shinyWidgets::pickerInput(), a
# real, actively-maintained CRAN package with its own binding name
# ("shinyWidgets.pickerInput"), its own CSS convention (".selectpicker"),
# and -- discovered while implementing this test, not assumed in advance --
# its own id-construction pattern (id attached via a separate
# htmltools::tagAppendAttributes() call, not passed into the initial tag()
# call the way every base Shiny input and the hand-rolled stand-in do it).
# That discovery is what led to patching tagAppendAttributes() in
# bootstrap.R alongside tag() -- this test locks that fix in.

test_that("real third-party widget (shinyWidgets::pickerInput): discovery + computed-id attribution", {
  skip_if_no_e2e()
  testthat::skip_if_not_installed("shinyWidgets")

  app_dir <- create_temp_app("e2e-widget-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shiny.cov::setup(app_dir)
  on.exit(try(shiny.cov::cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  app <- shiny.cov::AppDriver$new(
    app_dir,
    name = "e2e-widget-app",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE, after = FALSE)

  app$set_inputs(picker_1 = "y")
  # picker_2 deliberately left untouched, to confirm the merged coverage
  # value reflects only what was actually interacted with, not just what
  # was discovered.

  Sys.sleep(4) # let the periodic background save flush, as in test-e2e.R

  app$stop()

  cov <- shiny.cov::collect(app_dir)
  expect_s3_class(cov, "coverage")

  manifest_path <- file.path(app_dir, ".shiny.cov", "manifest.json")
  expect_true(file.exists(manifest_path))
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)

  input_ids <- vapply(manifest$inputs, function(x) x$id, character(1))
  input_types <- setNames(vapply(manifest$inputs, function(x) x$type, character(1)), input_ids)

  # Discovered via shinyWidgets' own binding registration, not any CSS
  # class shiny.cov knows about in advance.
  expect_true("picker_1" %in% input_ids)
  expect_true("picker_2" %in% input_ids)
  expect_equal(input_types[["picker_1"]], "shinyWidgets.pickerInput")

  # "picker_1"/"picker_2" have no literal string in the source (built
  # via `paste0("picker_", i)` in a loop) -- this synthetic coverage entry
  # can only exist at all if the tagAppendAttributes() hook found it.
  picker1_key <- grep("^ui:picker_1:", names(cov), value = TRUE)
  expect_gt(length(picker1_key), 0)
  expect_equal(cov[[picker1_key[[1]]]]$value, 1)

  picker2_key <- grep("^ui:picker_2:", names(cov), value = TRUE)
  expect_gt(length(picker2_key), 0)
  expect_equal(cov[[picker2_key[[1]]]]$value, 0)
})
