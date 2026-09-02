test_that("to_cobertura() writes one <line> per physical line (no method duplication)", {
  inst <- instrument_file(testthat::test_path("fixtures", "simple-app", "app.R"))
  env <- new.env()
  eval_instrumented(inst, env)

  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", "list")

  tmp <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp), add = TRUE)
  to_cobertura(cov, tmp)

  xml <- readLines(tmp, warn = FALSE)

  # No per-function method blocks (the codecov 100% bug).
  expect_false(any(grepl("<method ", xml, fixed = TRUE)))

  # The source file appears once, with its line-rate recorded.
  class_line <- grep('filename=', xml, value = TRUE)
  expect_length(class_line, 1)
  expect_match(class_line, 'line-rate="')

  # Both covered and uncovered lines are represented (this fixture has both:
  # ui/server definitions run, but the renderPlot() body is not called).
  expect_true(any(grepl('hits="0"', xml)))
  expect_true(any(grepl('hits="[1-9]', xml)))
})
