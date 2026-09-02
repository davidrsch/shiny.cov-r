test_that("source_coverage()/source_report() produce a per-source per-line table", {
  inst <- instrument_file(testthat::test_path("fixtures", "simple-app", "app.R"))
  env <- new.env()
  eval_instrumented(inst, env)
  counters <- as.list(inst$counters)

  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", "list")
  attr(cov, "shinycov_sources") <- list(cypress = counters, shinytest2 = counters)

  df <- source_coverage(cov)
  expect_true(is.data.frame(df))
  expect_true(all(c("filename", "line", "cypress", "shinytest2", "total") %in% names(df)))
  expect_gt(nrow(df), 0)
  # Each line's total equals the sum of its per-source columns.
  expect_equal(df$total, df$cypress + df$shinytest2)

  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(c(tmp, sub("\\.html$", ".json", tmp))), add = TRUE)
  expect_equal(source_report(cov, tmp), tmp)
  expect_true(file.exists(tmp))
  expect_true(file.exists(sub("\\.html$", ".json", tmp)))
  html <- readLines(tmp, warn = FALSE)
  expect_true(any(grepl("<table>", html, fixed = TRUE)))
})
