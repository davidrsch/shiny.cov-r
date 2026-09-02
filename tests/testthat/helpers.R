# Test helpers for shiny.cov

#' Create a temporary directory with app files for testing
#'
#' @param fixture_name One of "simple-app" or "multi-file-app".
#' @return Path to a temporary directory containing the test app.
create_temp_app <- function(fixture_name = "simple-app") {
  src <- testthat::test_path("fixtures", fixture_name)
  dst <- file.path(tempdir(), fixture_name)
  if (dir.exists(dst)) unlink(dst, recursive = TRUE)
  dir.create(dst, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), dst, recursive = TRUE)
  dst
}

#' Clean up a temporary app directory
cleanup_temp_app <- function(app_dir) {
  if (dir.exists(app_dir)) {
    unlink(app_dir, recursive = TRUE)
  }
}

#' Run setup in a temp directory and clean up after
with_test_setup <- function(app_dir, code) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_dir)

  setup(app_dir)
  on.exit(try(cleanup(app_dir), silent = TRUE), add = TRUE)

  force(code)
}

#' Check if covr is available (skip tests if not)
skip_if_no_covr <- function() {
  if (!requireNamespace("covr", quietly = TRUE)) {
    testthat::skip("covr not installed")
  }
}

#' Check if a headless-Chrome-capable end-to-end test can run
#'
#' Skips unless shiny/shinytest2/chromote are installed, a Chrome/Chromium
#' binary can be located, and shiny.cov itself is actually *installed*
#' (not just loaded via pkgload::load_all()) -- the bootstrap this test
#' exercises runs in a fresh child process that needs
#' requireNamespace("shiny.cov") to succeed there, which a dev-mode load in
#' this session does not guarantee.
skip_if_no_e2e <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  if (!nzchar(system.file(package = "shiny.cov"))) {
    testthat::skip("shiny.cov not installed (only dev-loaded) -- child process can't load it")
  }
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NA_character_)
  if (is.na(chrome) || !nzchar(chrome)) {
    testthat::skip("no Chrome/Chromium binary found")
  }
}
