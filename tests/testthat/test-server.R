# Tests for run_covr_server() (R/server.R) -- the Rscript -e entry point
# Playwright's webServer config invokes directly (see shiny.cov-playwright's
# README). shiny::runApp() itself blocks until the server stops, so these
# tests mock it out (testthat::local_mocked_bindings()) rather than actually
# starting a server -- what's under test here is env-var reading/validation
# and correct forwarding to runApp(), not runApp() itself.

#' Temporarily set/unset env vars for the duration of `code`, restoring
#' whatever was there before -- same manual save/restore pattern already
#' used in test-setup.R, not a new `withr` dependency for one test file.
#' @param vars Named character vector; `NA` means "unset this var".
with_env_vars <- function(vars, code) {
  old <- Sys.getenv(names(vars), unset = NA_character_, names = TRUE)
  on.exit({
    to_restore <- !is.na(old)
    if (any(to_restore)) do.call(Sys.setenv, as.list(old[to_restore]))
    to_unset <- names(vars)[is.na(old)]
    if (length(to_unset) > 0) Sys.unsetenv(to_unset)
  }, add = TRUE)

  to_set <- vars[!is.na(vars)]
  if (length(to_set) > 0) do.call(Sys.setenv, as.list(to_set))
  to_unset_now <- names(vars)[is.na(vars)]
  if (length(to_unset_now) > 0) Sys.unsetenv(to_unset_now)

  force(code)
}

test_that("errors clearly when SHINYCOV_SERVER_APP_DIR is not set", {
  with_env_vars(c(SHINYCOV_SERVER_APP_DIR = NA, SHINYCOV_SERVER_PORT = "3333"), {
    expect_error(run_covr_server(), "SHINYCOV_SERVER_APP_DIR")
  })
})

test_that("errors clearly when SHINYCOV_SERVER_PORT is not set", {
  with_env_vars(c(SHINYCOV_SERVER_APP_DIR = "/tmp/some-app", SHINYCOV_SERVER_PORT = NA), {
    expect_error(run_covr_server(), "SHINYCOV_SERVER_PORT")
  })
})

test_that("errors clearly when SHINYCOV_SERVER_PORT is not a valid integer", {
  with_env_vars(
    c(SHINYCOV_SERVER_APP_DIR = "/tmp/some-app", SHINYCOV_SERVER_PORT = "not-a-port"),
    {
      expect_error(run_covr_server(), "SHINYCOV_SERVER_PORT")
    }
  )
})

test_that("forwards appDir/port to shiny::runApp(), binding 127.0.0.1", {
  with_env_vars(
    c(SHINYCOV_SERVER_APP_DIR = "/tmp/some-app", SHINYCOV_SERVER_PORT = "4567"),
    {
      captured <- NULL
      testthat::local_mocked_bindings(
        runApp = function(...) captured <<- list(...),
        .package = "shiny"
      )

      run_covr_server()

      expect_equal(captured$appDir, "/tmp/some-app")
      expect_equal(captured$port, 4567L)
      expect_equal(captured$host, "127.0.0.1")
    }
  )
})

test_that("does not require SHINYCOV_OUTPUT/R_COVR to be set itself -- those are the caller's job", {
  # Mirrors shiny.cov-cypress/src/server.js's division of responsibility:
  # this function reads/validates only SHINYCOV_SERVER_APP_DIR/PORT and
  # calls runApp(); activating instrumentation via SHINYCOV_OUTPUT/R_COVR
  # is the webServer.env block's job (see README), not this function's.
  with_env_vars(
    c(
      SHINYCOV_SERVER_APP_DIR = "/tmp/some-app", SHINYCOV_SERVER_PORT = "4567",
      SHINYCOV_OUTPUT = NA, R_COVR = NA
    ),
    {
      captured <- NULL
      testthat::local_mocked_bindings(
        runApp = function(...) captured <<- list(...),
        .package = "shiny"
      )

      expect_no_error(run_covr_server())
      expect_equal(captured$appDir, "/tmp/some-app")
    }
  )
})
