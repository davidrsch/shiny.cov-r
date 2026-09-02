# .Last() shutdown-hook recursion regression test.
#
# bootstrap.R's `.Last <- ...` wrapper is meant to chain to any `.Last()`
# the app itself defined before bootstrap.R loads, while also running
# save_coverage() exactly once at real process exit. An earlier version
# captured the pre-existing .Last as a lazily-evaluated function-argument
# promise -- `(function(orig_last) { ... })(if (exists(".Last", mode =
# "function")) .Last else NULL)` -- which looks like it captures the prior
# value up front but doesn't: R never forces that promise until the
# returned closure's body actually references `orig_last`, which only
# happens at real R exit -- by which point the `.Last <-` assignment has
# already completed, so the lexical lookup of `.Last` the promise performs
# resolves to this very wrapper. The promise then resolves to itself, and
# `orig_last(...)` recurses into itself with no base case, until R's
# expression-nesting limit crashes the process with "Error: evaluation
# nested too deeply: infinite recursion" -- on ANY graceful exit path
# (q()/quit(), a Cypress-launched R subprocess exiting normally, an
# interactive session quitting), not just an edge case.
#
# Exercised with a real Rscript subprocess -- unlike the e2e-style tests
# elsewhere in this suite, no shiny session/chromote/browser is needed
# here: bootstrap.R's shutdown hooks fire from plain base R at process
# exit regardless of whether Shiny was ever loaded. This asserts on the
# subprocess's real exit status and stderr directly -- the same signal
# a real crash produces, and a 100%-reproducible one.

skip_if_bootstrap_not_installed <- function() {
  testthat::skip_on_cran()
  # The subprocess harness below (system2(Rscript --vanilla), asserting on
  # the child's exit status and on .Last()/reg.finalizer() ordering plus the
  # atomic file.rename on tempfiles) was written against POSIX semantics and
  # is not Windows-correct; the shutdown-hook behavior it guards is still
  # exercised by the Linux/macOS R CMD check jobs. Skipped here rather than
  # asserting on untested Windows-specific R shutdown behavior.
  testthat::skip_on_os("windows")
  # Mirrors skip_if_no_e2e() in helpers.R: the child process below needs
  # requireNamespace("shiny.cov") (bootstrap.R's own first real line of
  # logic) to succeed there, which a dev-mode pkgload::load_all() in this
  # session does not guarantee.
  if (!nzchar(system.file(package = "shiny.cov"))) {
    testthat::skip("shiny.cov not installed (only dev-loaded) -- child process can't load it")
  }
}

#' Run an R script fragment in a real Rscript subprocess
#'
#' @param body Character vector of R source lines, written to a temp `.R`
#'   file and run with `Rscript --vanilla`, so the child process starts
#'   from a clean, uninherited session.
#' @return A list with `status` (integer exit code, 0 if the process
#'   completed without `system2()` recording a nonzero one) and `output`
#'   (character vector of combined stdout+stderr lines).
run_rscript <- function(body) {
  script <- tempfile(fileext = ".R")
  writeLines(body, script)
  on.exit(unlink(script), add = TRUE)
  out <- suppressWarnings(
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script)),
      stdout = TRUE, stderr = TRUE
    )
  )
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, output = out)
}

test_that(".Last() shutdown hook does not recurse into itself on normal exit", {
  skip_if_bootstrap_not_installed()

  cov_file <- tempfile(fileext = ".rds")
  on.exit(unlink(cov_file), add = TRUE)

  # Minimal repro: set the env vars bootstrap.R gates its whole body on,
  # source it (which installs the .Last wrapper as a side effect), and
  # exit gracefully. A recursion regression here shows up as
  # quit(save = "no") alone crashing with exit status 1.
  #
  # A bare source() of bootstrap.R never runs any instrumented app code,
  # so .shinycov_counters (created by bootstrap.R at top level, landing in
  # .GlobalEnv since source() defaults to local = FALSE) stays empty --
  # save_coverage() intentionally no-ops on an empty counter set (see its
  # own early `if (length(counters) == 0) return()`), so nothing would get
  # written to cov_file even with a perfectly correct .Last(). Seed one
  # fake counter entry directly, after sourcing, so the exit-time
  # save_coverage() call inside .Last() has something real to persist --
  # this proves both that no crash happens AND that the save itself
  # genuinely completes, not just that the crash is avoided by silently
  # dropping the save.
  body <- c(
    sprintf('Sys.setenv(SHINYCOV_OUTPUT = %s, R_COVR = "true")', shQuote(cov_file)),
    "library(shiny.cov)",
    'source(system.file("bootstrap", "bootstrap.R", package = "shiny.cov"))',
    'assign("fake_key", 1L, envir = .shinycov_counters)',
    'quit(save = "no")'
  )

  res <- run_rscript(body)

  expect_equal(res$status, 0L)
  expect_false(any(grepl("nested too deeply", res$output, fixed = TRUE)))
  expect_true(file.exists(cov_file))
  saved <- readRDS(cov_file)
  expect_equal(saved[["fake_key"]], 1L)
})

test_that(".Last() shutdown hook still chains to a pre-existing .Last()", {
  skip_if_bootstrap_not_installed()

  # This is the actual feature the wrapper exists to implement -- calling
  # any .Last() the app itself already defined -- so avoiding the
  # recursion crash must not come at the cost of dropping the chaining
  # behavior outright.
  cov_file <- tempfile(fileext = ".rds")
  sentinel_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(cov_file, sentinel_file)), add = TRUE)

  body <- c(
    sprintf('Sys.setenv(SHINYCOV_OUTPUT = %s, R_COVR = "true")', shQuote(cov_file)),
    "library(shiny.cov)",
    # Defined *before* bootstrap.R is sourced, exactly like an app's own
    # app.R/global.R might define one.
    sprintf('.Last <- function() writeLines("prior .Last ran", %s)', shQuote(sentinel_file)),
    'source(system.file("bootstrap", "bootstrap.R", package = "shiny.cov"))',
    # See the sibling test above for why this is needed to exercise a real
    # save_coverage() write, not just the absence of a crash.
    'assign("fake_key", 1L, envir = .shinycov_counters)',
    'quit(save = "no")'
  )

  res <- run_rscript(body)

  expect_equal(res$status, 0L)
  expect_false(any(grepl("nested too deeply", res$output, fixed = TRUE)))
  expect_true(file.exists(sentinel_file))
  expect_equal(readLines(sentinel_file), "prior .Last ran")
  saved <- readRDS(cov_file)
  expect_equal(saved[["fake_key"]], 1L)
})

test_that("save_coverage() runs exactly once on graceful exit, not once from .Last() and again from reg.finalizer()", {
  skip_if_bootstrap_not_installed()

  # reg.finalizer()'s callback used to call save_coverage() unconditionally,
  # outside its own .shinycov_saved guard -- so a graceful exit ran it
  # twice (once via .Last(), once via the finalizer), each printing its own
  # "coverage saved" message. Not data-corrupting (save_coverage()'s
  # read-merge-write takes max() per counter key, so a second write is
  # idempotent) but wasteful and confusing. Count the message instead of
  # just checking the final file contents, since the file alone can't
  # distinguish "saved once" from "saved twice, second write a no-op".
  cov_file <- tempfile(fileext = ".rds")
  on.exit(unlink(cov_file), add = TRUE)

  body <- c(
    sprintf('Sys.setenv(SHINYCOV_OUTPUT = %s, R_COVR = "true")', shQuote(cov_file)),
    "library(shiny.cov)",
    'source(system.file("bootstrap", "bootstrap.R", package = "shiny.cov"))',
    'assign("fake_key", 1L, envir = .shinycov_counters)',
    'quit(save = "no")'
  )

  res <- run_rscript(body)

  expect_equal(res$status, 0L)
  save_messages <- grep("^shiny.cov: coverage saved", res$output, value = TRUE)
  expect_length(save_messages, 1)
})
