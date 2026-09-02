# End-to-end smoke test: runs a real fixture Shiny app as an actual
# subprocess, drives it with a real (headless) browser via shinytest2, and
# asserts on the real coverage.rds/manifest.json produced.
#
# Every other test in this suite calls instrument_file()/collect() directly
# on synthetic data; none of them ever run the generated app.R wrapper or
# bootstrap.R as a real child process. That's exactly how several bugs
# fixed alongside this test went undetected for a long time: bootstrap.R
# reimplementing (and silently diverging from) the package's own
# instrumentation/manifest logic, walk_ui() never actually accumulating
# results due to R's copy-on-modify list semantics, and instrument_file()
# only wrapping top-level statements (so a whole server function showed as
# "covered" the instant it was defined, regardless of which reactive
# branches a test actually triggered). This test exercises the whole
# pipeline for real so those classes of bug can't silently return.

test_that("real Shiny app: branch-level coverage + UI manifest + interaction log all work end-to-end", {
  skip_if_no_e2e()

  app_dir <- create_temp_app("e2e-app")
  # on.exit() handlers run in the order they were *registered* (first
  # registered = first run), not in reverse -- so cleanup_temp_app(), which
  # deletes the whole directory, must be registered *last* or it wipes
  # app_dir out from under app$stop()/shiny.cov::cleanup() before they get
  # a chance to run against a still-valid directory.
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shiny.cov::setup(app_dir)
  on.exit(try(shiny.cov::cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  app <- shiny.cov::AppDriver$new(
    app_dir,
    name = "e2e-app",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE, after = FALSE)

  # Trigger the "else" branch and the conditional panel.
  app$set_inputs(choice = "b")
  result_text <- app$get_text("#result")
  expect_equal(result_text, "Branch B")

  app$click(selector = 'a[data-value="About"]')
  app$click(input = "greet") # is this tracked as interacted?

  # Computed-id source-line attribution and real module-boundary detection:
  # "counter-bin_1" has no literal string in the source to text-search for
  # -- it's built via `ns(paste0("bin_", i))` inside a loop -- so this can
  # only be correctly merged/attributed via the htmltools::tag() +
  # moduleServer() hooks.
  app$click(selector = 'a[data-value="Counter"]')
  app$set_inputs(`counter-bin_1` = 7)

  # Background periodic saves matter here because graceful-shutdown hooks
  # are not reliably triggered by AppDriver$stop() -- a 20s graceful stop
  # with R_COVR=true does not fire onStop()/.Last()/reg.finalizer() at all
  # on this platform -- so coverage only reaches disk via the periodic
  # flush, every ~2s. Give at least one cycle time to run before reading
  # files.
  Sys.sleep(4)

  app$stop()

  cov <- shiny.cov::collect(app_dir)
  expect_s3_class(cov, "coverage")

  src_text_for <- function(entry) {
    sr <- entry$srcref
    if (!inherits(sr, "srcref")) return("")
    paste(as.character(sr), collapse = " ")
  }
  find_value <- function(needle) {
    hit <- Filter(function(e) grepl(needle, src_text_for(e), fixed = TRUE), cov)
    if (length(hit) == 0) return(NA_integer_)
    hit[[1]]$value
  }

  # Real branch-level granularity: only the branch actually taken (else /
  # "Branch B") should be hit; the untaken initial-render branch for the
  # default choice ("a") was hit once before we changed the input.
  expect_equal(find_value("\"Branch B\""), 1)
  expect_gte(find_value("\"Branch A\""), 0) # rendered once at session start with the default "a"

  manifest_path <- file.path(app_dir, ".shiny.cov", "manifest.json")
  expect_true(file.exists(manifest_path))
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)

  input_ids <- vapply(manifest$inputs, function(x) x$id, character(1))
  input_types <- setNames(vapply(manifest$inputs, function(x) x$type, character(1)), input_ids)
  # actionButton and the hand-registered custom binding both regression-test
  # discovery via Shiny's own binding registry, not CSS classes -- a
  # CSS-heuristic walker could never have found either of these.
  expect_setequal(
    input_ids,
    # "upload" (a fileInput) exercises upload_file(); see test-shinytest2.R
    # for the actual upload_file() coverage.
    c("choice", "greet", "weird", "upload", "counter-bin_1", "counter-bin_2")
  )
  expect_equal(input_types[["greet"]], "shiny.actionButtonInput")
  expect_equal(input_types[["weird"]], "demo.customWidget")

  expect_setequal(vapply(manifest$outputs, function(x) x$id, character(1)),
                   # "uploadedName" exercises upload_file(); see
                   # test-shinytest2.R.
                   c("result", "uploadedName", "extra", "about", "counter-total"))
  expect_setequal(unlist(manifest$tabs), c("Main", "About", "Counter"))
  expect_equal(unlist(manifest$conditional), "onlyForB")

  # Note: manifest.json itself (read raw, above) never carries module info --
  # that's written by the shiny::moduleServer() hook to a separate
  # modules.rds, and only applied on top of the manifest by
  # load_manifest()/apply_module_boundaries() (R/report.R), which
  # report()/ui_report() go through. See the ui_report() assertions below
  # for the module-boundary regression test.

  # "counter-bin_1" has no literal string in the source (it's built
  # via `ns(paste0("bin_", i))`), so this synthetic entry can only exist at
  # all if the htmltools::tag() hook's id -> srcref fallback found it.
  # Looked up by key (not text search) because "counter-bin_1" and
  # "counter-bin_2" are both constructed on the exact same source line (one
  # `lapply()` iteration each) and would otherwise be ambiguous to tell
  # apart by source text alone.
  bin1_key <- grep("^ui:counter-bin_1:", names(cov), value = TRUE)
  expect_gt(length(bin1_key), 0)
  expect_equal(cov[[bin1_key[[1]]]]$value, 1)
  bin2_key <- grep("^ui:counter-bin_2:", names(cov), value = TRUE)
  expect_gt(length(bin2_key), 0)
  expect_equal(cov[[bin2_key[[1]]]]$value, 0)

  interactions_path <- file.path(app_dir, ".shiny.cov", "interactions.json")
  expect_true(file.exists(interactions_path))
  interactions <- jsonlite::fromJSON(interactions_path, simplifyVector = FALSE)
  expect_true(any(vapply(interactions, function(x) identical(x$selector, "#choice"), logical(1))))

  ui <- shiny.cov::ui_report(cov, app_dir = app_dir)
  choice_entry <- Filter(function(x) x$id == "choice", ui$inputs)[[1]]
  expect_true(choice_entry$interacted)
  greet_entry <- Filter(function(x) x$id == "greet", ui$inputs)[[1]]
  expect_true(greet_entry$interacted) # discovered *and* tracked end-to-end
  weird_entry <- Filter(function(x) x$id == "weird", ui$inputs)[[1]]
  expect_false(weird_entry$interacted) # discovered, but never interacted with -- correctly so

  # Real module-boundary detection via shiny::moduleServer(), not a
  # post-hoc guess from splitting ids on "-". ui_report() goes through
  # load_manifest()/apply_module_boundaries(), unlike the raw manifest.json
  # read further up (which never carries module info -- see note above).
  bin1_entry <- Filter(function(x) x$id == "counter-bin_1", ui$inputs)[[1]]
  expect_equal(bin1_entry$module, "counter")
  expect_true(bin1_entry$interacted)
  expect_equal(choice_entry$module, "") # top-level, not inside any module
})
