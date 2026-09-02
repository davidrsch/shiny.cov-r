# Tests for R/shinytest2.R: the shiny.cov-aware AppDriver wrapper.
#
# Three behaviors covered here:
#  - log_interaction() writes to interactions.json atomically (tmp file
#    + rename) instead of a plain read-merge-overwrite, which could lose
#    updates from a concurrent AppDriver session or corrupt the file if the
#    process was killed mid-write.
#  - every logging AppDriver override logs *after* the underlying
#    shinytest2::AppDriver call succeeds, not before -- a thrown error must
#    not leave a phantom "interacted" entry behind.
#  - AppDriver overrides get_values()/expect_values()/run_js()/
#    wait_for_value(), so none of these are invisible to interaction
#    logging.
#
# The atomic-write behavior is tested directly against log_interaction(),
# independent of AppDriver, per the concurrency-testing pattern already
# established in test-e2e-concurrent.R (for coverage.rds) -- no browser
# needed. The other two need a real AppDriver against a real small Shiny
# app to verify honestly, so those reuse the fixture-app + skip_if_no_e2e()
# pattern from test-e2e.R.

# ---- log_interaction() atomic writes (no browser needed) ----

test_that("log_interaction() vocabulary: run_js is deliberately excluded from the allowed-actions lists", {
  # Documents/regresses the run_js() design decision: an arbitrary JS
  # string can't be honestly attributed to a specific input/output, so its
  # log entries must never be countable as a "hit" by merge_ui_coverage().
  expect_false("run_js" %in% shinycov_input_actions())
  expect_false("run_js" %in% shinycov_output_actions())
  # And the two new attributable read paths (get_values()/expect_values()/
  # wait_for_value()) reuse the *existing* vocabulary rather than inventing
  # new action names merge_ui_coverage() wouldn't recognize.
  expect_true("get_value" %in% shinycov_input_actions())
  expect_true("get_text" %in% shinycov_output_actions())
})

test_that("log_interaction() writes valid, appended JSON and leaves no tmp file behind", {
  log_dir <- tempfile("shiny.cov-log-")
  dir.create(log_dir)
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)
  log_path <- file.path(log_dir, "interactions.json")

  shinycov_set_interaction_log(log_path)
  on.exit(shinycov_set_interaction_log(NULL), add = TRUE)

  log_interaction("#a", "click")
  log_interaction("#b", "set_inputs", 42)

  expect_true(file.exists(log_path))
  # No leftover `<log_path>.tmp<pid>` -- file.rename() always completed.
  leftover_tmp <- list.files(log_dir, pattern = "\\.tmp[0-9]+$", full.names = TRUE)
  expect_length(leftover_tmp, 0)

  entries <- jsonlite::fromJSON(log_path, simplifyVector = FALSE)
  expect_length(entries, 2)
  expect_equal(entries[[1]]$selector, "#a")
  expect_equal(entries[[2]]$value, 42)
})

test_that("log_interaction() survives concurrent writers without corruption", {
  skip_on_os("windows") # parallel::mcparallel() needs fork(), unavailable on Windows
  skip_if_not_installed("parallel")

  log_dir <- tempfile("shiny.cov-log-concurrent-")
  dir.create(log_dir)
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)
  log_path <- file.path(log_dir, "interactions.json")

  n_per_worker <- 25
  worker <- function(tag) {
    shinycov_set_interaction_log(log_path)
    for (i in seq_len(n_per_worker)) {
      log_interaction(paste0("#", tag, "-", i), "click")
    }
    TRUE
  }

  # Two real forked processes, both writing to the same interactions.json
  # concurrently -- exactly the "two AppDriver sessions against the same
  # app_dir" scenario this guards, minus the browser overhead.
  job_a <- parallel::mcparallel(worker("a"))
  job_b <- parallel::mcparallel(worker("b"))
  res <- parallel::mccollect(list(job_a, job_b), wait = TRUE)
  expect_true(all(vapply(res, isTRUE, logical(1))))

  leftover_tmp <- list.files(log_dir, pattern = "\\.tmp[0-9]+$", full.names = TRUE)
  expect_length(leftover_tmp, 0)

  # The file must always be valid, parseable JSON -- the atomic
  # tmp-then-rename write means readers only ever see a complete previous
  # version or a complete new one, never a half-written/truncated one, even
  # with two writers racing on it the whole time.
  final <- jsonlite::fromJSON(log_path, simplifyVector = FALSE)
  expect_type(final, "list")

  # The atomic write narrows the lost-update race (per the documented
  # trade-off, matching bootstrap.R's save_coverage()) but does NOT
  # eliminate it: two processes can still both read the same on-disk
  # snapshot before either writes back, and whichever renames second wins
  # outright, discarding the other's newest entry. The merge *outcome* is
  # therefore scheduling-dependent and non-deterministic -- under perfect
  # fork interleaving it can even be a clean last-writer-wins where one
  # worker's entire contribution is lost -- so this test must not assert a
  # particular number or prefix of surviving entries (a probabilistic
  # outcome that intermittently fails under CI load). What the atomic
  # tmp-then-rename write *does* guarantee, and what's asserted here: the
  # file is never corrupted/truncated (the successful parse above) and is
  # never empty or malformed, regardless of how the two forks interleave.
  expect_gt(length(final), 0)
  expect_true(all(vapply(final, function(x) {
    is.character(x$selector) && nzchar(x$selector) &&
      is.character(x$action) && nzchar(x$action)
  }, logical(1))))
})

# ---- real AppDriver behavior (needs a live browser) ----

test_that("AppDriver: new get_values()/expect_values()/run_js()/wait_for_value() overrides log correctly, and a failed call logs nothing", {
  skip_if_no_e2e()

  app_dir <- create_temp_app("e2e-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shiny.cov::setup(app_dir)
  on.exit(try(shiny.cov::cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  app <- shiny.cov::AppDriver$new(
    app_dir,
    name = "shinytest2-overrides",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE, after = FALSE)

  interactions_path <- file.path(app_dir, ".shiny.cov", "interactions.json")
  read_log <- function() {
    if (!file.exists(interactions_path)) return(list())
    jsonlite::fromJSON(interactions_path, simplifyVector = FALSE)
  }
  count_matching <- function(log, selector, action) {
    sum(vapply(log, function(x) {
      identical(x$selector, selector) && identical(x$action, action)
    }, logical(1)))
  }

  # ---- get_values(): logs one get_value per input id, one get_text per
  # output id present in the result. ----
  before <- read_log()
  values <- app$get_values()
  expect_true("choice" %in% names(values$input))
  expect_true("result" %in% names(values$output))
  after <- read_log()
  expect_gt(
    count_matching(after, "#choice", "get_value"),
    count_matching(before, "#choice", "get_value")
  )
  expect_gt(
    count_matching(after, "#result", "get_text"),
    count_matching(before, "#result", "get_text")
  )

  # ---- expect_values(): same logging contract, driven off the snapshotted
  # JSON content (its return value) rather than a parsed list, since
  # shinytest2::AppDriver$expect_values() returns the raw JSON string, not
  # get_values()'s parsed list. screenshot_args = FALSE skips the
  # screenshot comparison -- irrelevant to what's being tested here and
  # slower/flakier under headless Chrome. ----
  # This test is about the logging side effect, not about pinning down the
  # app's exact JSON output as a long-lived golden file, so don't leave a
  # snapshot artifact behind for future runs to maintain -- remove whatever
  # directory testthat's snapshot machinery creates for this test file
  # (name-mangled from the active test_that() description, not just
  # `snap_name`, so clean up by directory rather than guessing the exact
  # generated filename).
  #
  # The default snapshotter *fails* on CI when a snapshot is new (see
  # testthat::snapshot_file_equal()'s `fail_on_new = on_ci()` default), and
  # this test's snapshot is deliberately never committed, so it would always
  # be "new" and fail under R CMD check / CI. A local snapshotter with
  # `fail_on_new = FALSE` turns that into a warning instead, letting the
  # snapshot be created and then deleted below while still exercising the
  # real expect_values() snapshot path.
  testthat::local_snapshotter(
    snap_dir = testthat::test_path("_snaps"),
    fail_on_new = FALSE
  )
  snap_dir <- testthat::test_path("_snaps", "shinytest2")
  on.exit(unlink(snap_dir, recursive = TRUE), add = TRUE)
  snap_name <- "shinytest2-overrides-expect-values"

  before <- read_log()
  expect_values_ok <- tryCatch({
    app$expect_values(screenshot_args = FALSE, name = snap_name)
    TRUE
  }, error = function(e) {
    message("expect_values() failed: ", conditionMessage(e))
    FALSE
  })
  expect_true(expect_values_ok)
  after <- read_log()
  expect_gt(
    count_matching(after, "#choice", "get_value"),
    count_matching(before, "#choice", "get_value")
  )
  expect_gt(
    count_matching(after, "#result", "get_text"),
    count_matching(before, "#result", "get_text")
  )

  # ---- run_js(): logs exactly one non-attributable "window"/"run_js"
  # entry, no matter what the script does -- it must never be attributed to
  # a specific input/output (see the roxygen comment on run_js() for why).
  # Uses Shiny.setInputValue() directly, the real-world reason run_js() is
  # in shinytest2 test suites at all. ----
  before <- read_log()
  app$run_js("Shiny.setInputValue('choice', 'b', {priority: 'event'});")
  app$wait_for_idle()
  after <- read_log()
  new_entries <- after[seq(length(before) + 1, length(after))]
  run_js_entries <- Filter(function(x) identical(x$action, "run_js"), new_entries)
  expect_length(run_js_entries, 1)
  expect_equal(run_js_entries[[1]]$selector, "window")
  expect_true(grepl("setInputValue", run_js_entries[[1]]$value, fixed = TRUE))
  # run_js is intentionally not part of the coverage-counted vocabulary, so
  # even though it just changed `choice` under the hood, it alone must not
  # be what makes the UI report call `choice` "interacted" -- reset to "a"
  # via set_inputs() first so this specific assertion is only about run_js.
  app$set_inputs(choice = "a")
  interactions_reset_idx <- length(read_log())
  app$run_js("Shiny.setInputValue('greet', 999, {priority: 'event'});")
  app$wait_for_idle()
  log_after_second_run_js <- read_log()
  # "greet" must not show up as a set_inputs/click hit purely from run_js.
  expect_equal(
    count_matching(log_after_second_run_js, "#greet", "set_inputs") +
      count_matching(log_after_second_run_js, "#greet", "click"),
    0
  )

  # ---- wait_for_value(): logs exactly one entry (get_value for an input
  # target, get_text for an output target), even though shinytest2's own
  # implementation polls by calling self$get_value() repeatedly under the
  # hood (dispatched through R6's self$ back into this class's own
  # overrides) -- without suppressing logging during that inner loop, a
  # single wait_for_value() call would log once per poll iteration instead
  # of once. ----
  before <- read_log()
  val <- app$wait_for_value(input = "choice", timeout = 4000)
  expect_equal(val, "a")
  after <- read_log()
  new_entries <- after[seq(length(before) + 1, length(after))]
  expect_length(Filter(function(x) identical(x$action, "get_value") &&
                          identical(x$selector, "#choice"), new_entries), 1)

  before <- read_log()
  val <- app$wait_for_value(output = "result", timeout = 4000)
  expect_true(is.character(val))
  after <- read_log()
  new_entries <- after[seq(length(before) + 1, length(after))]
  expect_length(Filter(function(x) identical(x$action, "get_text") &&
                          identical(x$selector, "#result"), new_entries), 1)

  # ---- A failed call must not log anything. ----
  before <- read_log()
  expect_error(app$click(selector = "#this-selector-matches-nothing-at-all"))
  after <- read_log()
  expect_equal(length(before), length(after))

  # wait_for_value() times out (and throws) if the value never stops
  # matching `ignore` -- `choice` is genuinely "a" and never changes here,
  # so ignoring "a" guarantees a timeout. This also regresses the
  # suppress_log on.exit() cleanup in wait_for_value(): if the flag were
  # left TRUE after an error (e.g. reset only on the happy path instead of
  # via on.exit()), every subsequent interaction for the rest of the
  # AppDriver's life would silently stop being logged at all.
  before <- read_log()
  expect_error(
    app$wait_for_value(input = "choice", ignore = list("a"), timeout = 800, interval = 200)
  )
  after <- read_log()
  expect_equal(length(before), length(after))

  # Confirm logging still works normally after that error (suppress_log
  # was correctly reset, not left stuck TRUE).
  before <- read_log()
  app$get_value(input = "choice")
  after <- read_log()
  expect_gt(length(after), length(before))
})

# ---- upload_file() / click(output = ...) / get_value(output = ...)|
# get_value(export = ...) ----
#
# Each override's signature must match the real installed
# shinytest2::AppDriver method, not an assumed shape:
#  - upload_file()'s override must forward via the real single *named*
#    dynamic-dots argument (`upload_file(myInputId = "path")`, matching
#    `function(..., wait_ = TRUE, timeout_ = missing_arg())`), not
#    positional `upload_file(selector, file, ...)` -- the latter throws
#    "Can only upload file to exactly one input, and input must be named"
#    against the real method.
#  - click()'s override must recognize `output` as a valid target, not
#    just `input`/`selector` -- otherwise `target <- input %||% selector`
#    evaluates to NULL for a `click(output = "id")` call (the call itself
#    still succeeds, falling through `...` to shinytest2's own click()),
#    and log_interaction(NULL, "click") writes a malformed empty-list
#    "selector" into interactions.json, which later crashes
#    merge_ui_coverage() in R/collect.R for the *entire app*.
#  - get_value()'s override must accept `input`/`output`/`export` the same
#    way the real method does (`function(..., input = missing_arg(),
#    output = missing_arg(), export = missing_arg(), hash_images =
#    FALSE)`) -- a plain *required* positional `input` argument throws
#    "argument \"input\" is missing, with no default" the instant
#    `get_value(output = "id")` or `get_value(export = "id")` is called,
#    before ever reaching super$.
test_that("AppDriver: upload_file()/click(output=)/get_value(output=|export=) all work and log correctly", {
  skip_if_no_e2e()

  app_dir <- create_temp_app("e2e-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shiny.cov::setup(app_dir)
  on.exit(try(shiny.cov::cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  app <- shiny.cov::AppDriver$new(
    app_dir,
    name = "shinytest2-upload-click-output-get-value",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE, after = FALSE)

  interactions_path <- file.path(app_dir, ".shiny.cov", "interactions.json")
  read_log <- function() {
    if (!file.exists(interactions_path)) return(list())
    jsonlite::fromJSON(interactions_path, simplifyVector = FALSE)
  }
  count_matching <- function(log, selector, action) {
    sum(vapply(log, function(x) {
      identical(x$selector, selector) && identical(x$action, action)
    }, logical(1)))
  }

  # ---- upload_file() must accept the real single-named-dynamic-dots
  # call convention and actually succeed. Also confirms the app really did
  # receive the file (not just that the R call didn't throw). ----
  upload_path <- tempfile(fileext = ".txt")
  writeLines("hello from shiny.cov", upload_path)
  on.exit(unlink(upload_path), add = TRUE)

  before <- read_log()
  expect_no_error(app$upload_file(upload = upload_path))
  app$wait_for_idle()
  after <- read_log()
  expect_gt(
    count_matching(after, "#upload", "upload_file"),
    count_matching(before, "#upload", "upload_file")
  )
  new_entries <- after[seq(length(before) + 1, length(after))]
  upload_entries <- Filter(function(x) identical(x$action, "upload_file"), new_entries)
  expect_length(upload_entries, 1)
  expect_equal(upload_entries[[1]]$value, upload_path)

  uploaded_name <- app$wait_for_value(output = "uploadedName", timeout = 4000)
  expect_equal(uploaded_name, basename(upload_path))

  # ---- click(output = ...) must log a proper string selector (not a
  # malformed empty-list one), and merge_ui_coverage()/collect() must
  # not choke on the resulting interactions.json. ----
  before <- read_log()
  expect_no_error(app$click(output = "result"))
  after <- read_log()
  new_entries <- after[seq(length(before) + 1, length(after))]
  click_entries <- Filter(function(x) identical(x$action, "click"), new_entries)
  expect_length(click_entries, 1)
  expect_true(is.character(click_entries[[1]]$selector))
  expect_equal(click_entries[[1]]$selector, "#result")

  # ---- get_value(output = ...) must not throw, and must log a
  # get_text entry (mirroring how an `input` target logs get_value). ----
  before <- read_log()
  result_value <- app$get_value(output = "result")
  expect_true(is.character(result_value))
  after <- read_log()
  expect_gt(
    count_matching(after, "#result", "get_text"),
    count_matching(before, "#result", "get_text")
  )

  # ---- get_value(export = ...) must not throw either. Exports aren't
  # part of the UI manifest coverage tracks, so nothing should be logged for
  # it (mirrors wait_for_value()'s documented export behavior). ----
  before <- read_log()
  export_value <- app$get_value(export = "currentChoice")
  expect_equal(export_value, "a")
  after <- read_log()
  expect_equal(length(before), length(after))

  # Background periodic saves flush coverage.rds every ~2s (see test-e2e.R
  # for why AppDriver$stop() alone isn't relied on for this); give at least
  # one cycle before reading files.
  Sys.sleep(4)
  app$stop()

  # Confirm collect() runs clean end-to-end over everything logged in this
  # test, including the click(output=...) entry above -- the malformed-
  # selector crash site, exercised for real rather than just unit-tested
  # against a hand-written interactions.json (test-merge-ui-coverage-
  # collision.R covers that angle directly).
  expect_no_error(cov <- shiny.cov::collect(app_dir))
  expect_s3_class(cov, "coverage")
})

# ---- click() with a non-scalar target must fail with shinytest2's own
# clear validation error, not a confusing base-R one ----
#
# click()'s override builds `target` from whichever of input/output/
# selector was supplied, then only massages it into a "#id" selector when
# it's a single, non-NA string (see the `length(target) == 1` guard
# above) -- anything else, including a length-2+ character vector, is
# left untouched and falls through to super$click() unchanged. Without
# that guard, `grepl("^#", target)` on a length-2+ vector returns a
# length-2+ logical, and `if()` coercing that to a single logical throws
# R's own low-level "'length = 2' in coercion to 'logical(1)'" error
# before shinytest2's own validation (`ckm8_assert_single_string()`,
# invoked deep inside super$click() via app_find_node_id()/
# node_id_css_selector()) ever gets a chance to fire its clear "Must have
# length 1, but has length N" message instead.
test_that("AppDriver: click() with a non-scalar input/output/selector surfaces shinytest2's own clear error, not a confusing base-R coercion error", {
  skip_if_no_e2e()

  app_dir <- create_temp_app("e2e-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shiny.cov::setup(app_dir)
  on.exit(try(shiny.cov::cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  app <- shiny.cov::AppDriver$new(
    app_dir,
    name = "shinytest2-click-non-scalar-target",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE, after = FALSE)

  interactions_path <- file.path(app_dir, ".shiny.cov", "interactions.json")
  read_log <- function() {
    if (!file.exists(interactions_path)) return(list())
    jsonlite::fromJSON(interactions_path, simplifyVector = FALSE)
  }

  check_clear_error <- function(err) {
    expect_s3_class(err, "error")
    msg <- conditionMessage(err)
    # The old bug's signature message -- must NOT appear anymore.
    expect_false(grepl("coercion", msg, fixed = TRUE))
    expect_false(grepl("length = 2' in coercion", msg, fixed = TRUE))
    # shinytest2's own ckm8_assert_single_string() message mentions length.
    expect_true(grepl("length", msg, ignore.case = TRUE))
  }

  before <- read_log()
  check_clear_error(tryCatch(app$click(input = c("choice", "greet")),
                              error = function(e) e))
  check_clear_error(tryCatch(app$click(output = c("result", "choice")),
                              error = function(e) e))
  check_clear_error(tryCatch(app$click(selector = c("#a", "#b")),
                              error = function(e) e))
  after <- read_log()

  # None of the three failed calls should have logged anything (the same
  # "a thrown error must not leave a phantom entry behind" contract every
  # logging override follows).
  expect_equal(length(before), length(after))

  # A normal, scalar click() must still work and still log correctly --
  # confirms the added length/type guard doesn't interfere with the
  # ordinary, already-covered case.
  expect_no_error(app$click(input = "greet"))
  final <- read_log()
  click_entries <- Filter(function(x) identical(x$action, "click"), final)
  expect_true(any(vapply(click_entries, function(x) identical(x$selector, "#greet"),
                          logical(1))))
})
