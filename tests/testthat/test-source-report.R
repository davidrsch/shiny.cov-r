test_that("report() writes a self-contained HTML with per-source line counts", {
  inst <- instrument_file(testthat::test_path("fixtures", "simple-app", "app.R"))
  env <- new.env()
  eval_instrumented(inst, env)
  counters <- as.list(inst$counters)

  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", "list")
  attr(cov, "shinycov_sources") <- list(cypress = counters, shinytest2 = counters)

  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  f <- report(cov, file = tmp, app_dir = tempdir())
  expect_true(file.exists(f))

  html <- readLines(f, warn = FALSE)
  # Per-source gutter annotation is present.
  expect_true(any(grepl("cypress=", html, fixed = TRUE)))
  expect_true(any(grepl("shinytest2=", html, fixed = TRUE)))
  # Uncovered lines are highlighted.
  expect_true(any(grepl("uncovered", html, fixed = TRUE)))
})
