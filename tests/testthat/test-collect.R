skip_if_no_covr()

test_that("collect() errors on missing coverage file", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  expect_error(
    collect(app_dir),
    "Coverage file not found"
  )
})

test_that("collect() returns a coverage object from valid data", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  # Set up manually: write a coverage RDS with known data
  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  outfile <- file.path(app_dir, ".shiny.cov", "coverage.rds")

  # Create mock counters in the format covr uses
  app_path <- file.path(app_dir, "app.R")
  app_path <- normalizePath(app_path, winslash = "/", mustWork = TRUE)

  # Instrument the file to get real counters
  inst <- instrument_file(app_path)

  # Save counters as an RDS
  counters_list <- as.list(inst$counters)
  saveRDS(counters_list, outfile)

  # Collect
  cov <- collect(app_dir)
  expect_s3_class(cov, "coverage")
  expect_true(length(cov) > 0)
})

test_that("collect() returns empty coverage for empty RDS", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  outfile <- file.path(app_dir, ".shiny.cov", "coverage.rds")

  # Save an empty list
  saveRDS(list(), outfile)

  cov <- collect(app_dir)
  expect_s3_class(cov, "coverage")
  expect_equal(length(cov), 0)
})

test_that("reconstruct_counters() builds correct environment", {
  env <- new.env(parent = emptyenv())
  assign("test.R:1:1:1:10:1:10", list(value = 5), envir = env)
  assign("test.R:2:1:2:10:2:10", list(value = 3), envir = env)

  reconstructed <- reconstruct_counters(as.list(env))
  expect_true(is.environment(reconstructed))

  keys <- ls(reconstructed, all.names = TRUE)
  expect_equal(length(keys), 2)
  expect_true("test.R:1:1:1:10:1:10" %in% keys)
})

test_that("build_coverage() produces a coverage object", {
  # Create a valid counter entry by instrumenting a file
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(c("hello <- function() { message('hi') }", "hello()"), tmp)

  inst <- instrument_file(tmp)
  cov <- build_coverage(inst$counters)

  expect_type(cov, "list")
  expect_true(length(cov) > 0)
  # Each element should have $value
  for (entry in cov) {
    expect_true("value" %in% names(entry))
  }
})

# ---- readRDS() failure includes the original conditionMessage() ----
test_that("collect() includes the original error message when readRDS() fails", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  outfile <- file.path(app_dir, ".shiny.cov", "coverage.rds")
  # Not a valid RDS file at all -- readRDS() will throw, and the resulting
  # conditionMessage() (something like "unknown input format") must survive
  # into collect()'s rethrown error rather than being discarded, so a user
  # can tell a truncated/wrong-format file apart from e.g. a permissions
  # error.
  writeLines("this is not an RDS file", outfile)

  original_message <- tryCatch(
    readRDS(outfile),
    error = function(e) conditionMessage(e)
  )
  expect_true(is.character(original_message))

  expect_error(
    collect(app_dir),
    paste0("Failed to read coverage file.*", "\\Q", original_message, "\\E"),
    perl = TRUE
  )
})

# ---- malformed coverage.rds entries are reported, not silently dropped ----
test_that("reconstruct_counters() messages and skips entries that are neither list nor numeric", {
  counters <- list(
    "good" = list(value = 5),
    "bad" = "not a valid counter entry"
  )
  expect_message(
    reconstructed <- reconstruct_counters(counters),
    "skipped 1 malformed counter entry"
  )
  keys <- ls(reconstructed, all.names = TRUE)
  expect_equal(keys, "good")
})

test_that("reconstruct_counters() stays silent when nothing is malformed", {
  counters <- list("good" = list(value = 5))
  expect_no_message(reconstruct_counters(counters))
})

test_that("build_coverage() messages and skips entries with a missing srcref", {
  env <- new.env(parent = emptyenv())
  assign("no_srcref", list(value = 3), envir = env) # no $srcref at all
  expect_message(
    result <- build_coverage(env),
    "skipped 1 counter entry.*missing/malformed srcref"
  )
  expect_equal(length(result), 0)
})

test_that("build_coverage() stays silent when every entry has a srcref", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(c("hello <- function() { message('hi') }", "hello()"), tmp)
  inst <- instrument_file(tmp)
  expect_no_message(build_coverage(inst$counters))
})

test_that("collect() surfaces a diagnostic message when coverage.rds has malformed entries", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  outfile <- file.path(app_dir, ".shiny.cov", "coverage.rds")

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  counters_list <- as.list(inst$counters)
  # Simulate the kind of corruption/format-drift this guards against: a stored
  # counter entry that's neither a list nor a bare numeric.
  counters_list[["corrupted_entry"]] <- "not a valid counter"
  saveRDS(counters_list, outfile)

  expect_message(
    cov <- collect(app_dir),
    "shiny.cov: skipped 1 malformed counter entry"
  )
  expect_s3_class(cov, "coverage")
})

# ---- regex_escape() neutralizes every regex metacharacter ----
test_that("regex_escape() neutralizes every base R regex metacharacter", {
  metachars <- c(".", "\\", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
  for (ch in metachars) {
    id <- paste0("a", ch, "b")
    escaped <- regex_escape(id)
    pattern <- sprintf("^%s$", escaped)
    # Must still match itself literally, wrapped as an anchored pattern...
    expect_true(grepl(pattern, id), info = paste("metachar:", ch))
    # ...and must NOT match a different "a?b"-shaped string -- if it did,
    # the metachar was left with its regex meaning instead of being
    # neutralized to a literal.
    expect_false(grepl(pattern, "aXb"), info = paste("metachar:", ch))
  }

  # A combined id with every metacharacter at once (the adversarial case)
  # must not throw "invalid regular expression" and must still round-trip
  # as a literal match, embedded exactly the way locate_by_text() embeds it.
  nasty <- ".+*?()[]{}^$|\\"
  nasty_id <- paste0("id", nasty, "end")
  escaped <- regex_escape(nasty_id)
  pattern <- sprintf('["\']%s["\']', escaped)
  expect_no_error(matched <- grepl(pattern, paste0('"', nasty_id, '"')))
  expect_true(matched)
  expect_false(grepl(pattern, '"idXend"'))
})

test_that("collect() output is a valid coverage object", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  outfile <- file.path(app_dir, ".shiny.cov", "coverage.rds")

  app_path <- file.path(app_dir, "app.R")
  inst <- instrument_file(app_path)
  saveRDS(as.list(inst$counters), outfile)

  cov <- collect(app_dir)
  expect_s3_class(cov, "coverage")
  # Coverage object should have entries for files
  expect_true(length(cov) > 0)
  # Entries should have $value
  for (entry in cov) {
    expect_true("value" %in% names(entry))
  }
})
