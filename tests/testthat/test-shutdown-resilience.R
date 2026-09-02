# Shutdown-resilience regression test.
#
# bootstrap.R documents graceful shutdown hooks (onStop()/.Last()/
# reg.finalizer()) as "best-effort" (see that file's comments around the
# periodic-save registration): a graceful AppDriver$stop() with
# R_COVR=true does not reliably trigger any of them on this platform.
# The periodic background save (`later::later(..., delay =
# 2)`, roughly every ~2s) is what the docs call "the primary defense against
# data loss" -- yet every other e2e test in this suite (test-e2e.R,
# test-e2e-concurrent.R) only ever exercises the *graceful* shutdown path via
# app$stop(). None of them simulate the actual failure mode the periodic
# save exists to survive: the child R process disappearing abruptly, with no
# chance to run shutdown hooks at all.
#
# This test does that for real: spawns the fixture app as an actual child R
# process (via shiny.cov::AppDriver, which -- like shinytest2::AppDriver --
# spawns the app with callr::r_bg() under the hood; test-e2e-concurrent.R is
# the closest existing precedent for treating that spawned process as a real,
# independent OS process rather than just an AppDriver object with a single
# blessed stop() path), drives one real, attributable interaction, waits
# comfortably longer than one periodic-save cycle, and then reaches past the
# AppDriver object entirely to SIGKILL the underlying process -- bypassing
# app$stop() and every graceful-shutdown code path -- before asserting
# shiny.cov::collect() still recovers the interaction.

test_that("periodic background save recovers coverage across an ungraceful SIGKILL", {
  skip_if_no_e2e()

  app_dir <- create_temp_app("e2e-app")
  # Registration order matters for on.exit() (first registered = first run,
  # not reverse) -- cleanup_temp_app() deletes app_dir outright, so it must
  # be registered *last* or it wipes app_dir out from under
  # shiny.cov::cleanup() before that gets a chance to run against a
  # still-valid directory. Same convention as test-e2e.R/test-e2e-concurrent.R.
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shiny.cov::setup(app_dir)
  on.exit(try(shiny.cov::cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  app <- shiny.cov::AppDriver$new(
    app_dir,
    name = "shutdown-resilience",
    timeout = 30000,
    load_timeout = 30000
  )
  # Deliberately NOT `on.exit(app$stop())` as the primary teardown for the
  # process itself -- the whole point of this test is that the process gets
  # killed before any graceful stop() ever runs. A `try()`-wrapped stop() is
  # still registered below as a safety net for the chromote/browser side (in
  # case an assertion fails before the real kill happens further down); it's
  # a no-op for the R process once that's already been SIGKILLed.
  on.exit(try(app$stop(), silent = TRUE), add = TRUE, after = FALSE)

  # One real, attributable interaction: trigger the "else" branch of
  # output$result's if/else (default is choice = "a" / "Branch A"; this
  # exercises "Branch B", which only ever executes as a result of this
  # set_inputs() call -- exactly the same repro shape test-e2e.R already
  # uses to prove a specific piece of coverage is attributable to a specific
  # interaction, not just "something happened at some point").
  app$set_inputs(choice = "b")
  result_text <- app$get_text("#result")
  expect_equal(result_text, "Branch B")

  # Give at least one periodic-save cycle (~2s per bootstrap.R) time to run
  # *before* we kill the process -- same 4s figure test-e2e.R and
  # test-e2e-concurrent.R already use for this exact reason, kept consistent
  # rather than picking a new number. Anything that happened before this
  # wait-then-kill sequence is within what the periodic save promises to
  # have captured; bootstrap.R's own comment is explicit that an abrupt kill
  # only loses "at most a couple of seconds of the most recent interaction",
  # not everything that happened up to ~2s ago.
  Sys.sleep(4)

  # Reach past the AppDriver object to the real OS process shinytest2's
  # callr::r_bg() spawned for it (private$shiny_process, an r_process/
  # processx::process object -- test-e2e-concurrent.R stops at the
  # AppDriver-object level of "real separate child process"; this test goes
  # one level further and manipulates that process directly), and send it a
  # real SIGKILL. processx::process$kill() sends SIGKILL unconditionally on
  # this platform (its `grace` argument is documented as "currently not
  # used" -- verified directly: a killed process's $get_exit_status() here
  # reports -9). This is not app$stop(); no shutdown hook in bootstrap.R
  # (onStop()/.Last()/reg.finalizer()) gets a chance to run at all, since
  # none of them can intercept SIGKILL.
  shiny_process <- app$.__enclos_env__$private$shiny_process
  expect_true(shiny_process$is_alive())
  shiny_process$kill()
  # Give the OS a moment to finish tearing the process down before reading
  # the coverage file it wrote for the last time.
  Sys.sleep(0.5)
  expect_false(shiny_process$is_alive())

  cov <- shiny.cov::collect(app_dir)
  expect_s3_class(cov, "coverage")

  # Same lookup-by-srcref-text helper test-e2e.R and test-e2e-concurrent.R
  # use, with one deliberate change: those tests pick `hit[[1]]` (the first
  # text match), which is safe there because *every* matching entry
  # (the single "Branch B" line and the coarser enclosing
  # renderText()/server() statements whose concatenated text also contains
  # that substring) always agrees on value in the graceful-shutdown case.
  # That's not true here: this test asserts on a scenario where the coarse
  # enclosing entries are always 1 (captured by the early, definition-time
  # save that runs regardless of the periodic-save mechanism under test)
  # while the fine-grained single-line entry is the only one that actually
  # reflects whether the *periodic* save captured the interaction --
  # `hit[[1]]` alone would pick the always-1 whole-function entry and make
  # this test pass even with the periodic-save registration in bootstrap.R
  # disabled, making it a false-positive regression test. Picking the match
  # with the shortest source text selects the specific single-line entry
  # instead.
  src_text_for <- function(entry) {
    sr <- entry$srcref
    if (!inherits(sr, "srcref")) return("")
    paste(as.character(sr), collapse = " ")
  }
  find_value <- function(needle) {
    hit <- Filter(function(e) grepl(needle, src_text_for(e), fixed = TRUE), cov)
    if (length(hit) == 0) return(NA_integer_)
    lens <- vapply(hit, function(e) nchar(src_text_for(e)), integer(1))
    hit[[which.min(lens)]]$value
  }

  # The actual claim under test: despite the process being SIGKILLed with no
  # graceful shutdown of any kind, the interaction from before the 4s wait
  # survived via the periodic background save alone. This is exactly the
  # scenario bootstrap.R calls out as the periodic save's reason to exist,
  # and until this test, nothing exercised it.
  expect_equal(find_value("\"Branch B\""), 1)
})
