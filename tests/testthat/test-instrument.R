skip_if_no_covr()

test_that("instrument_file() returns a list with exprs and counters", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  app_path <- file.path(app_dir, "app.R")
  result <- instrument_file(app_path)

  expect_type(result, "list")
  expect_true("exprs" %in% names(result))
  expect_true("counters" %in% names(result))
  expect_type(result$exprs, "list")
  expect_true(is.environment(result$counters))
})

test_that("instrument_file() produces non-zero counter entries", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  app_path <- file.path(app_dir, "app.R")
  result <- instrument_file(app_path)

  # The app.R fixture has multiple expressions that should produce counters
  n <- length(ls(result$counters, all.names = TRUE))
  expect_gt(n, 0)
})

test_that("instrument_file() errors on non-existent file", {
  expect_error(
    instrument_file("/nonexistent/file.R"),
    "File not found"
  )
})

test_that("instrumented expressions can be evaluated", {
  # Create a minimal R file
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(c("x <- 1 + 1", "y <- x * 2"), tmp)

  # Instrument
  result <- instrument_file(tmp)

  # Evaluate in a clean environment using eval_instrumented
  env <- new.env(parent = baseenv())
  eval_instrumented(result, env)

  # Check that expressions executed correctly
  expect_equal(env$x, 2)
  expect_equal(env$y, 4)
})

test_that("instrument_code() works with in-memory code", {
  code <- c("a <- 1", "b <- a + 1")
  result <- instrument_code(code)

  expect_type(result, "list")
  expect_true("exprs" %in% names(result))
  expect_true("counters" %in% names(result))

  # Evaluate
  env <- new.env(parent = baseenv())
  eval_instrumented(result, env)
  expect_equal(env$a, 1)
  expect_equal(env$b, 2)
})

test_that("instrument_file() handles multi-file app (nested source)", {
  app_dir <- create_temp_app("multi-file-app")
  on.exit(cleanup_temp_app(app_dir))

  # Instrument the main app.R
  app_path <- file.path(app_dir, "app.R")
  result <- instrument_file(app_path)
  expect_gte(length(result$exprs), 1)

  # Instrument the helpers.R separately
  helpers_path <- file.path(app_dir, "helpers.R")
  result2 <- instrument_file(helpers_path)
  expect_gte(length(result2$exprs), 1)
})

test_that("instrument_file() gives real per-branch coverage, not just per-top-level-statement", {
  # Regression test: instrument_file() used to wrap only *top-level*
  # expressions with a single count() call, so an entire function body --
  # including all its internal branches -- was marked "covered" the
  # instant it was *defined*, and calling it any number of times with any
  # combination of branches never changed anything. covr:::trace_calls()
  # must be used so each branch gets its own counter that only increments
  # when that branch actually executes.
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(c(
    "f <- function(x) {",
    "  if (x > 0) {",
    "    \"positive\"",
    "  } else {",
    "    \"negative\"",
    "  }",
    "}",
    "for (i in 1:10) f(5)",
    "f(-5)"
  ), tmp)

  result <- instrument_file(tmp)
  env <- new.env(parent = baseenv())
  eval_instrumented(result, env)

  keys <- ls(result$counters, all.names = TRUE)
  # More than one counter means the function body was actually recursed
  # into (real AST tracing), not just wrapped as a single top-level unit.
  expect_gt(length(keys), 1)

  # A counter spanning multiple lines (e.g. the whole `f <- function(x) {...}`
  # definition) deparses to *all* of those lines, so a naive substring match
  # for "positive" would also match the enclosing, multi-line counter -- not
  # just the single-line leaf counter for the branch itself. Restrict to
  # single-line counters to find the precise leaf.
  is_single_line <- function(k) {
    sr <- get(k, envir = result$counters)$srcref
    inherits(sr, "srcref") && sr[[1]] == sr[[3]]
  }
  src_text_for <- function(k) {
    sr <- get(k, envir = result$counters)$srcref
    if (is.null(sr)) return("")
    paste(as.character(sr), collapse = " ")
  }
  value_for <- function(k) get(k, envir = result$counters)$value

  single_line_keys <- Filter(is_single_line, keys)
  positive_key <- single_line_keys[vapply(single_line_keys, function(k) grepl("positive", src_text_for(k), fixed = TRUE), logical(1))]
  negative_key <- single_line_keys[vapply(single_line_keys, function(k) grepl("negative", src_text_for(k), fixed = TRUE), logical(1))]

  expect_length(positive_key, 1)
  expect_length(negative_key, 1)

  # The positive branch executed 10 times (via the loop), the negative
  # branch exactly once -- these must differ, and must reflect the actual
  # call counts, not just "1" (definition-time only).
  expect_equal(value_for(positive_key), 10)
  expect_equal(value_for(negative_key), 1)
})

test_that("eval_instrumented() evaluates all expressions", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(c("result1 <- 10", "result2 <- result1 * 3", "result3 <- result2 + 5"), tmp)

  result <- instrument_file(tmp)
  env <- new.env(parent = baseenv())
  eval_instrumented(result, env)

  expect_equal(env$result1, 10)
  expect_equal(env$result2, 30)
  expect_equal(env$result3, 35)
})
