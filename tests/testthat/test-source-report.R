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
  # Uncovered lines are highlighted (covr uses "missed"; fallback uses "uncovered").
  expect_true(any(grepl("missed", html, fixed = TRUE)) || any(grepl("uncovered", html, fixed = TRUE)))
})

test_that("render_source_table annotates hit counts with per-source breakdown", {
  skip_if_not_installed("htmltools")
  full <- list(`/f/app.R` = data.frame(
    line = c(1L, 2L, 3L),
    source = c("x <- 1", "y <- 2", "# comment"),
    coverage = c("1", "0", ""),
    stringsAsFactors = FALSE))
  src <- data.frame(filename = "/f/app.R", line = c(1L, 2L),
                    cypress = c(1L, 0L), shinytest2 = c(1L, 0L),
                    total = c(2L, 0L))
  html <- htmltools::renderTags(
    render_source_table(full, src, c("cypress", "shinytest2")))$html
  expect_true(grepl("cypress=1 shinytest2=1", html, fixed = TRUE))
  expect_true(grepl("cypress=0 shinytest2=0", html, fixed = TRUE))
  # The never-tracked comment line gets no annotation.
  expect_equal(lengths(regmatches(html, gregexpr("cypress=", html, fixed = TRUE))), 2L)
})
