# Concurrent-session regression test.
#
# Two AppDriver instances against the *same* app_dir means two separate
# child R processes, each with its own .shinycov_counters environment,
# each periodically flushing coverage.rds independently with no knowledge
# of the other. If save_coverage() (inst/bootstrap/bootstrap.R) overwrote
# coverage.rds outright with just the current process's own counters, the
# wrong save timing would let whichever process saves last silently erase
# the other's real, already-executed coverage -- it instead merges with
# whatever's already on disk (max per key) before writing.
#
# This test can't force a specific bad save-timing interleaving (there's
# no hook to freeze one child process's clock from here), but it
# does force the *order* most likely to expose it: the session with the
# unique contribution is stopped first, and the session that never
# touched that code path is stopped last -- so if its own (stale, zero)
# state ever clobbers the file instead of merging, this fails.

test_that("two concurrent AppDriver sessions against the same app_dir don't clobber each other's coverage", {
  skip_if_no_e2e()

  app_dir <- create_temp_app("e2e-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shiny.cov::setup(app_dir)
  on.exit(try(shiny.cov::cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  app1 <- shiny.cov::AppDriver$new(app_dir, name = "concurrent-1", timeout = 30000, load_timeout = 30000)
  on.exit(try(app1$stop(), silent = TRUE), add = TRUE, after = FALSE)
  app2 <- shiny.cov::AppDriver$new(app_dir, name = "concurrent-2", timeout = 30000, load_timeout = 30000)
  on.exit(try(app2$stop(), silent = TRUE), add = TRUE, after = FALSE)

  # Only app2 ever exercises Branch B; app1 never changes `choice` away
  # from its default. app2 is stopped first, app1 last -- app1's own final
  # save (which only ever saw Branch B at value 0) must not erase app2's.
  app2$set_inputs(choice = "b")
  Sys.sleep(4)
  app2$stop()

  Sys.sleep(2)
  app1$stop()

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

  expect_gte(find_value("\"Branch B\""), 1)
  expect_gte(find_value("\"Branch A\""), 1)
})
