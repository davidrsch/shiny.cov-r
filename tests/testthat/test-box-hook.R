# {box}/rhino apps load every module file through box:::load_from_source(),
# not base::source() -- the source() override in bootstrap.R never sees
# this code at all. shiny.cov:::install_box_hook() (R/box_hook.R) patches
# load_from_source() itself so box modules get the same
# shinycov_trace_calls() branch-level instrumentation.
#
# These tests call shiny.cov:::install_box_hook() directly -- the real
# function that ships inside the package and that bootstrap.R's template
# now just calls, rather than hand-duplicating the patch logic inline (an
# earlier version of this file did that, using the superseded
# covr:::trace_calls()/covr:::.counters approach -- see R/trace.R's header
# for why that was abandoned in favor of shiny.cov's own in-tree-ported
# tracing engine). That meant this file used to test a mechanism that
# wasn't what actually shipped; it now exercises the real shipped code
# path, including its two defensive guards against {box} internal-API
# drift (see test-e2e.R for the full real-browser rhino scenario).

#' Save box:::load_from_source, returning a function that restores it
#'
#' Call the returned function (typically via `on.exit()`) to put the
#' original binding back, unlock/assign/lock -- mirroring exactly how
#' `install_box_hook()` itself installs a replacement.
save_load_from_source <- function(box_ns) {
  orig <- box_ns$load_from_source
  function() {
    unlockBinding("load_from_source", box_ns)
    assign("load_from_source", orig, envir = box_ns)
    lockBinding("load_from_source", box_ns)
  }
}

#' Force box:::load_from_source to a given value (function or not)
set_load_from_source <- function(box_ns, value) {
  unlockBinding("load_from_source", box_ns)
  assign("load_from_source", value, envir = box_ns)
  lockBinding("load_from_source", box_ns)
}

test_that("install_box_hook() gives real branch-level coverage for box modules", {
  testthat::skip_if_not_installed("box")

  mod_dir <- file.path(tempdir(), "shinycov_box_test_mod")
  dir.create(mod_dir, showWarnings = FALSE)
  on.exit(unlink(mod_dir, recursive = TRUE), add = TRUE)
  writeLines(c(
    "#' @export",
    "classify <- function(age) {",
    "  if (age < 18) {",
    "    \"Young\"",
    "  } else {",
    "    \"Adult\"",
    "  }",
    "}"
  ), file.path(mod_dir, "logic.R"))

  counter_env <- new.env(parent = emptyenv())
  old_counters <- shiny.cov:::shinycov_set_counters(counter_env)
  on.exit(shiny.cov:::shinycov_set_counters(old_counters), add = TRUE)

  box_ns <- asNamespace("box")
  restore <- save_load_from_source(box_ns)
  on.exit(restore(), add = TRUE)

  result <- shiny.cov:::install_box_hook()
  expect_true(isTRUE(result))

  old_box_path <- getOption("box.path")
  options(box.path = tempdir())
  on.exit(options(box.path = old_box_path), add = TRUE)

  box::use(shinycov_box_test_mod / logic)
  on.exit(try(box::unload(logic), silent = TRUE), add = TRUE)

  expect_equal(logic$classify(42), "Adult")

  keys <- ls(counter_env, all.names = TRUE)
  expect_gt(length(keys), 1) # real per-branch tracing, not one counter for the whole function

  src_text_for <- function(k) {
    sr <- get(k, envir = counter_env)$srcref
    if (is.null(sr)) return("")
    paste(as.character(sr), collapse = " ")
  }
  value_for <- function(k) get(k, envir = counter_env)$value
  is_single_line <- function(k) {
    sr <- get(k, envir = counter_env)$srcref
    inherits(sr, "srcref") && sr[[1]] == sr[[3]]
  }

  single_line_keys <- Filter(is_single_line, keys)
  adult_key <- single_line_keys[vapply(single_line_keys, function(k) grepl("Adult", src_text_for(k), fixed = TRUE), logical(1))]
  young_key <- single_line_keys[vapply(single_line_keys, function(k) grepl("Young", src_text_for(k), fixed = TRUE), logical(1))]

  expect_length(adult_key, 1)
  expect_length(young_key, 1)
  expect_equal(value_for(adult_key), 1) # classify(42) took this branch
  expect_equal(value_for(young_key), 0) # never called with age < 18
})

test_that("install_box_hook() skips gracefully when load_from_source isn't a function", {
  testthat::skip_if_not_installed("box")

  box_ns <- asNamespace("box")
  restore <- save_load_from_source(box_ns)
  on.exit(restore(), add = TRUE)

  # Simulate {box} internal-API drift: `$` on a namespace environment
  # returns NULL for a missing/renamed binding, not an error -- this
  # reproduces that exact silent-failure shape, which is what the guard in
  # install_box_hook() exists to catch (rather than building a broken stub
  # from NULL and installing it in place of the real loader).
  set_load_from_source(box_ns, NULL)

  expect_message(
    result <- shiny.cov:::install_box_hook(),
    "was not found as a function"
  )
  expect_false(isTRUE(result))

  # Nothing broken/half-installed in place of the (currently missing)
  # real loader.
  expect_null(box_ns$load_from_source)

  # Restore the real loader and confirm box::use() itself still works
  # normally afterward -- uninstrumented, but functional, not broken.
  restore()

  mod_dir <- file.path(tempdir(), "shinycov_box_test_mod_missing_api")
  dir.create(mod_dir, showWarnings = FALSE)
  on.exit(unlink(mod_dir, recursive = TRUE), add = TRUE)
  writeLines(c(
    "#' @export",
    "greet <- function() \"hello\""
  ), file.path(mod_dir, "greeter_missing_api.R"))

  old_box_path <- getOption("box.path")
  options(box.path = tempdir())
  on.exit(options(box.path = old_box_path), add = TRUE)

  box::use(shinycov_box_test_mod_missing_api / greeter_missing_api)
  on.exit(try(box::unload(greeter_missing_api), silent = TRUE), add = TRUE)
  expect_equal(greeter_missing_api$greet(), "hello")
})

test_that("install_box_hook() skips gracefully when load_from_source's body doesn't match the expected shape", {
  testthat::skip_if_not_installed("box")

  box_ns <- asNamespace("box")
  restore <- save_load_from_source(box_ns)
  on.exit(restore(), add = TRUE)

  orig <- box_ns$load_from_source
  expect_true(is.function(orig)) # sanity: guard 1 wouldn't be what trips here

  # Simulate {box} restructuring load_from_source() internally so the
  # exact `eval(exprs, mod_ns)` statement install_box_hook() surgically
  # replaces no longer has the shape it expects (here: a call to eval()
  # with a named `envir=` argument instead of positional -- structurally
  # different, not identical()).
  mismatched <- orig
  body(mismatched)[[3]] <- quote(eval(exprs, envir = mod_ns))
  set_load_from_source(box_ns, mismatched)

  expect_message(
    result <- shiny.cov:::install_box_hook(),
    "did not match the expected shape"
  )
  expect_false(isTRUE(result))

  # Left exactly as it was -- still the mismatched-but-real function, not
  # patched, not corrupted.
  expect_identical(box_ns$load_from_source, mismatched)

  # Restore the real loader and confirm box::use() itself still works
  # normally afterward -- uninstrumented, but functional, not broken.
  restore()

  mod_dir <- file.path(tempdir(), "shinycov_box_test_mod_mismatch")
  dir.create(mod_dir, showWarnings = FALSE)
  on.exit(unlink(mod_dir, recursive = TRUE), add = TRUE)
  writeLines(c(
    "#' @export",
    "greet <- function() \"hi there\""
  ), file.path(mod_dir, "greeter_mismatch.R"))

  old_box_path <- getOption("box.path")
  options(box.path = tempdir())
  on.exit(options(box.path = old_box_path), add = TRUE)

  box::use(shinycov_box_test_mod_mismatch / greeter_mismatch)
  on.exit(try(box::unload(greeter_mismatch), silent = TRUE), add = TRUE)
  expect_equal(greeter_mismatch$greet(), "hi there")
})
