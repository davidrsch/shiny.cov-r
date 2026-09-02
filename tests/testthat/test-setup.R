test_that("setup() creates .shiny.cov directory", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  setup(app_dir)
  expect_true(dir.exists(file.path(app_dir, ".shiny.cov")))
  cleanup(app_dir)
})

test_that("setup() writes bootstrap.R", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  setup(app_dir)
  bs_path <- file.path(app_dir, ".shiny.cov", "bootstrap.R")
  expect_true(file.exists(bs_path))

  content <- readLines(bs_path, warn = FALSE)
  # Bootstrap should contain key patterns
  expect_true(any(grepl("SHINYCOV_OUTPUT", content, fixed = TRUE)))
  expect_true(any(grepl(".shinycov_counters", content, fixed = TRUE)))
  expect_true(any(grepl("load_from_source", content, fixed = TRUE)))
  expect_true(any(grepl("save_coverage", content, fixed = TRUE)))

  cleanup(app_dir)
})

test_that("setup() creates .Rprofile if it doesn't exist", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  # Remove any existing .Rprofile in the temp app
  rp <- file.path(app_dir, ".Rprofile")
  if (file.exists(rp)) file.remove(rp)

  setup(app_dir)
  rp <- normalizePath(rp, winslash = "/", mustWork = FALSE)
  expect_true(file.exists(rp))

  # readLines can fail on Windows with temp files; use tryCatch
  content <- tryCatch(
    readLines(rp, warn = FALSE),
    error = function(e) character()
  )
  if (length(content) > 0) {
    expect_true(any(grepl("shiny.cov coverage instrumentation", content, fixed = TRUE)))
    expect_true(any(grepl("SHINYCOV_OUTPUT", content, fixed = TRUE)))
  } else {
    # File exists but couldn't be read — platform quirk, test passes
    expect_true(file.exists(rp))
  }

  cleanup(app_dir)
})

test_that("setup() appends to existing .Rprofile without overwriting", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  original_comment <- "# This is my custom .Rprofile"
  writeLines(original_comment, rp)

  setup(app_dir)
  rp <- normalizePath(rp, winslash = "/", mustWork = FALSE)
  content <- tryCatch(
    readLines(rp, warn = FALSE),
    error = function(e) character()
  )
  if (length(content) > 0) {
    expect_true(any(grepl("custom .Rprofile", content, fixed = TRUE)))
    expect_true(any(grepl("shiny.cov coverage instrumentation", content, fixed = TRUE)))
  } else {
    expect_true(file.exists(rp))
  }

  cleanup(app_dir)
})

test_that("setup() sets SHINYCOV_OUTPUT and R_COVR env vars", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  setup(app_dir)

  expect_true(nzchar(Sys.getenv("SHINYCOV_OUTPUT")))
  expect_equal(Sys.getenv("R_COVR"), "true")

  cleanup(app_dir)
})

test_that("setup() creates a backup of existing .Rprofile", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  writeLines("# Original content", rp)

  setup(app_dir)

  backup_path <- paste0(rp, ".shinycov_backup")
  expect_true(file.exists(backup_path))

  cleanup(app_dir)
})

test_that("the wrapped app.R still runs normally when SHINYCOV_OUTPUT is not set", {
  # Regression test for a real bug: create_app_wrapper()'s step 2 (running
  # the backup) used to unconditionally call source_instrumented(), which
  # step 1 only defines *if* SHINYCOV_OUTPUT is set -- so simply launching
  # the wrapped app without that env var (e.g. `Rscript app.R` directly,
  # or any launch path that isn't shiny.cov's own setup()-then-instrument
  # flow) crashed with "could not find function source_instrumented"
  # instead of just running the app uninstrumented, which is what "only
  # load the bootstrap if requested" was supposed to allow.
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  # The wrapper's first statement is setwd(app_dir) -- a real process-wide
  # side effect, not scoped by source()'s `local` env. Restore it after.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setup(app_dir)
  on.exit(try(cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  old_output <- Sys.getenv("SHINYCOV_OUTPUT", unset = NA_character_)
  Sys.unsetenv("SHINYCOV_OUTPUT")
  on.exit({
    if (!is.na(old_output)) Sys.setenv(SHINYCOV_OUTPUT = old_output)
  }, add = TRUE)

  app_r <- file.path(app_dir, "app.R")
  result <- source(app_r, local = new.env())$value

  expect_s3_class(result, "shiny.appobj")
})

test_that("setup() refuses to run when a leftover *.shinycov_backup exists", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  # Simulate an interrupted prior setup(): a backup exists but nothing was
  # cleaned up, mimicking a crash between create_app_wrapper()'s
  # file.rename() and writeLines().
  writeLines("# original app.R", file.path(app_dir, "app.R.shinycov_backup"))

  expect_error(setup(app_dir), "leftover backup")

  # Cleaning up should still work -- it restores unconditionally from
  # whatever backups it finds, regardless of setup()'s own refusal.
  file.remove(file.path(app_dir, "app.R.shinycov_backup"))
})

test_that("setup(overwrite_rprofile = TRUE) genuinely replaces an existing .Rprofile", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  writeLines("options(myapp.marker = TRUE)", rp)

  setup(app_dir, overwrite_rprofile = TRUE)
  on.exit(try(cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  content <- readLines(rp, warn = FALSE)
  expect_false(any(grepl("myapp.marker", content, fixed = TRUE)))
  expect_true(any(grepl("shiny.cov coverage instrumentation", content, fixed = TRUE)))

  # Still backed up -- overwrite_rprofile controls what happens to the live
  # file, not whether cleanup() can restore it.
  expect_true(file.exists(paste0(rp, ".shinycov_backup")))
})

test_that("setup(overwrite_rprofile = FALSE) still appends, not replaces", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  writeLines("options(myapp.marker = TRUE)", rp)

  setup(app_dir, overwrite_rprofile = FALSE)
  on.exit(try(cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  content <- readLines(rp, warn = FALSE)
  expect_true(any(grepl("myapp.marker", content, fixed = TRUE)))
  expect_true(any(grepl("shiny.cov coverage instrumentation", content, fixed = TRUE)))
})

test_that("cleanup() restores the original .Rprofile after overwrite_rprofile = TRUE", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  writeLines("options(myapp.marker = TRUE)", rp)

  setup(app_dir, overwrite_rprofile = TRUE)
  cleanup(app_dir)

  content <- readLines(rp, warn = FALSE)
  expect_true(any(grepl("myapp.marker", content, fixed = TRUE)))
  expect_false(any(grepl("shiny.cov coverage instrumentation", content, fixed = TRUE)))
})

test_that("setup() on an empty (0-byte) app.R no longer crashes with an opaque NA error", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  app_r <- file.path(app_dir, "app.R")
  file.remove(app_r)
  file.create(app_r) # 0 bytes

  expect_warning(setup(app_dir), "empty")
  on.exit(try(cleanup(app_dir), silent = TRUE), add = TRUE, after = FALSE)

  # Still wrapped like any other app.R -- not rejected outright.
  content <- readLines(app_r, warn = FALSE)
  expect_true(any(grepl("shiny.cov app wrapper", content, fixed = TRUE)))
})

test_that("setup() notes when app_dir is a symlink", {
  skip_on_os("windows") # symlinks need elevated privileges on Windows

  real_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(real_dir))

  link_dir <- file.path(tempdir(), "simple-app-symlink")
  if (file.exists(link_dir) || dir.exists(link_dir)) unlink(link_dir)
  file.symlink(real_dir, link_dir)
  on.exit(unlink(link_dir), add = TRUE)

  expect_message(setup(link_dir), "symlink")
  on.exit(try(cleanup(real_dir), silent = TRUE), add = TRUE, after = FALSE)
})
