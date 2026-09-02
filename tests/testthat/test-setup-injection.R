# Regression tests for a path-injection risk in setup.R: paths (ultimately
# derived from the caller-controlled `app_dir`) get spliced into generated
# R source. If that splice used sprintf('...("%s")...') with zero
# escaping, a path containing an embedded `"` could break out of the
# string literal and run arbitrary R when the generated app.R / .Rprofile
# was source()d. Every such splice instead routes through
# encodeString(path, quote = '"').
#
# These tests mirror the exploit shape: a directory name crafted to look
# like `evil"); file.create("<marker>"); #`
# which, if unescaped, turns `setwd("<path>")` into
# `setwd("evil"); file.create("<marker>"); #")` -- i.e. arbitrary code
# execution. We assert the marker file is NOT created.

test_that("setup()'s generated app.R does not execute code injected via a crafted app_dir", {
  testthat::skip_on_os("windows")
  base <- tempdir()
  marker <- file.path(base, paste0("shinycov_pwned_marker_", Sys.getpid()))
  if (file.exists(marker)) file.remove(marker)

  # Directory name containing both a `"` (breaks out of the R string
  # literal) and a `\` (an attacker's classic trick to try to defeat naive
  # escaping by eating an escape character) immediately followed by a
  # statement separator and a trailing `#` to comment out the rest of the
  # generated line -- the same shape as the confirmed exploit.
  evil_name <- sprintf('evil\\app"); file.create("%s"); #', marker)
  app_dir <- file.path(base, evil_name)
  if (dir.exists(app_dir)) unlink(app_dir, recursive = TRUE)
  dir.create(app_dir, recursive = TRUE)

  old_wd <- getwd()
  on.exit(
    {
      setwd(old_wd)
      try(cleanup(app_dir), silent = TRUE)
      if (file.exists(marker)) file.remove(marker)
      if (dir.exists(app_dir)) unlink(app_dir, recursive = TRUE)
    },
    add = TRUE
  )

  file.copy(
    testthat::test_path("fixtures", "simple-app", "app.R"),
    file.path(app_dir, "app.R")
  )

  setup(app_dir)

  app_r <- file.path(app_dir, "app.R")
  content <- readLines(app_r, warn = FALSE)
  # Sanity check: the crafted path really did make it into the generated
  # wrapper (properly escaped, so we match on a substring unaffected by
  # escaping rather than the raw basename) -- otherwise this test would
  # pass for the wrong reason (e.g. setup() silently failing).
  expect_true(any(grepl("evil", content, fixed = TRUE)))
  expect_true(any(grepl(basename(base), content, fixed = TRUE)))

  # SHINYCOV_OUTPUT is not set here, so only the unconditional setwd()
  # line at the top of the wrapper runs -- exactly the line that was
  # vulnerable and exactly what the original exploit used.
  old_output <- Sys.getenv("SHINYCOV_OUTPUT", unset = NA_character_)
  Sys.unsetenv("SHINYCOV_OUTPUT")
  on.exit(
    {
      if (!is.na(old_output)) Sys.setenv(SHINYCOV_OUTPUT = old_output)
    },
    add = TRUE
  )

  result <- source(app_r, local = new.env())$value

  # The wrapper must still work as a normal Shiny app despite the unusual
  # characters in its path...
  expect_s3_class(result, "shiny.appobj")
  # ...but the injected file.create() call must NOT have executed.
  expect_false(file.exists(marker))
})

test_that("inject_rprofile()'s generated .Rprofile does not execute code injected via a crafted bootstrap path", {
  testthat::skip_on_os("windows")
  base <- tempdir()
  marker <- file.path(base, paste0("shinycov_pwned_rprofile_marker_", Sys.getpid()))
  if (file.exists(marker)) file.remove(marker)

  evil_name <- sprintf('evilrp\\app"); file.create("%s"); #', marker)
  app_dir <- file.path(base, evil_name)
  if (dir.exists(app_dir)) unlink(app_dir, recursive = TRUE)
  dir.create(app_dir, recursive = TRUE)

  on.exit(
    {
      try(cleanup(app_dir), silent = TRUE)
      if (file.exists(marker)) file.remove(marker)
      if (dir.exists(app_dir)) unlink(app_dir, recursive = TRUE)
    },
    add = TRUE
  )

  file.copy(
    testthat::test_path("fixtures", "simple-app", "app.R"),
    file.path(app_dir, "app.R")
  )

  setup(app_dir)

  rprofile_path <- file.path(app_dir, ".Rprofile")
  expect_true(file.exists(rprofile_path))

  content <- readLines(rprofile_path, warn = FALSE)
  expect_true(any(grepl("evilrp", content, fixed = TRUE)))
  expect_true(any(grepl(basename(base), content, fixed = TRUE)))

  # We deliberately do NOT set SHINYCOV_OUTPUT here, so the guarded
  # `tryCatch(source(bs_path), ...)` block is skipped at *evaluation*
  # time -- but that's not what matters for this test. R parses an
  # entire script up front before evaluating any of it, so a syntax
  # error from an unescaped `"` breaking out of the string literal would
  # make source() fail here regardless of which branch is taken -- an
  # injected `"` in the generated .Rprofile raises a parse error
  # immediately, before the `if` condition is ever evaluated. A clean,
  # error-free source() call is therefore sufficient (and safer than
  # actually loading bootstrap.R -- which installs process-wide shiny
  # hooks not meant to run inside a shared test session -- would be) to
  # prove the sprintf() site inside inject_rprofile() correctly escapes
  # its input.
  old_output <- Sys.getenv("SHINYCOV_OUTPUT", unset = NA_character_)
  Sys.unsetenv("SHINYCOV_OUTPUT")
  on.exit(
    {
      if (!is.na(old_output)) Sys.setenv(SHINYCOV_OUTPUT = old_output)
    },
    add = TRUE
  )

  expect_no_error(source(rprofile_path, local = new.env()))
  expect_false(file.exists(marker))
})
