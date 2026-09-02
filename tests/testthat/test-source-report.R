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

test_that("render_source_table adds per-source columns with a fixed header", {
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
  # fixed header with one column per source
  expect_true(grepl("<thead>", html, fixed = TRUE))
  expect_true(grepl("<th class=\"source-col\">cypress</th>", html, fixed = TRUE))
  expect_true(grepl("<th class=\"source-col\">shinytest2</th>", html, fixed = TRUE))
  expect_true(grepl("<th class=\"coverage\">count</th>", html, fixed = TRUE))
  # per-source cell values (covered line: 1/1; missed line: 0/0)
  expect_true(grepl("<td class=\"source-col\">1</td>", html, fixed = TRUE))
  expect_true(grepl("<td class=\"source-col\">0</td>", html, fixed = TRUE))
})

test_that("covr-style report keeps DataTables callback valid (no lost backslashes)", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  inst <- instrument_file(testthat::test_path("fixtures", "simple-app", "app.R"))
  env <- new.env()
  eval_instrumented(inst, env)
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", "list")
  attr(cov, "shinycov_sources") <- list(cypress = as.list(inst$counters), shinytest2 = as.list(inst$counters))

  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  render_report_html_covr(cov, NULL, tmp)
  ln <- grep("files.filter", readLines(tmp, warn = FALSE), value = TRUE)
  frag <- regmatches(ln, regexec("files.filter[^\"]*", ln))[[1]]
  # The JS selector must keep its escaped quotes; a dropped backslash would
  # leave '' (empty strings), a JS syntax error that blanks the Files table.
  expect_false(grepl("''", frag, fixed = TRUE))
  expect_true(grepl("\\\\", frag))
})
