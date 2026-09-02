test_that("cleanup() restores .Rprofile from backup", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  original_content <- c("# My custom settings", "options(stringsAsFactors = FALSE)")
  writeLines(original_content, rp)

  setup(app_dir)

  # Verify snippet was appended
  content_after_setup <- readLines(rp, warn = FALSE)
  expect_true(any(grepl("shiny.cov", content_after_setup, fixed = TRUE)))

  cleanup(app_dir)

  # After cleanup, .Rprofile should be restored
  content_after_cleanup <- readLines(rp, warn = FALSE)
  expect_false(any(grepl("shiny.cov", content_after_cleanup, fixed = TRUE)))
  expect_equal(content_after_cleanup, original_content)
})

test_that("cleanup() removes .shiny.cov directory", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  setup(app_dir)
  expect_true(dir.exists(file.path(app_dir, ".shiny.cov")))

  cleanup(app_dir)
  expect_false(dir.exists(file.path(app_dir, ".shiny.cov")))
})

test_that("cleanup() unsets environment variables", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  setup(app_dir)
  expect_true(nzchar(Sys.getenv("SHINYCOV_OUTPUT")))

  cleanup(app_dir)
  expect_equal(Sys.getenv("SHINYCOV_OUTPUT"), "")
  expect_equal(Sys.getenv("R_COVR"), "")
})

test_that("cleanup() removes snippet even without backup", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  writeLines("# Line before\n\n# Line after", rp)

  setup(app_dir)

  # Remove the backup to simulate missing backup scenario
  backup_path <- paste0(rp, ".shinycov_backup")
  if (file.exists(backup_path)) file.remove(backup_path)

  # This should still work (warning about missing backup)
  expect_warning(
    cleanup(app_dir),
    NA  # The function itself doesn't warn — it handles gracefully
  )

  # Snippet should be removed (auto-detection)
  content <- readLines(rp, warn = FALSE)
  expect_false(any(grepl("shiny.cov", content, fixed = TRUE)))
})

test_that("cleanup() is safe to call when nothing is set up", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  # No setup() called — cleanup should still work without error
  expect_error(cleanup(app_dir), NA)
})

test_that("cleanup() handles non-existent app_dir gracefully", {
  expect_error(cleanup("/nonexistent/path"), NA)
})

test_that("cleanup() removes .Rprofile entirely when setup() created it from scratch", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  rp <- file.path(app_dir, ".Rprofile")
  if (file.exists(rp)) file.remove(rp)

  setup(app_dir)
  expect_true(file.exists(rp)) # setup() created it fresh, no backup

  cleanup(app_dir)

  # Fully restores the pre-setup() state (no .Rprofile at all), not a
  # stray empty file.
  expect_false(file.exists(rp))
})

test_that("cleanup() does not crash on a 0-byte app.R with no backup", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  app_r <- file.path(app_dir, "app.R")
  file.remove(app_r)
  file.create(app_r) # 0 bytes, no ".shinycov_backup" sibling

  expect_error(cleanup(app_dir), NA)
})

test_that("cleanup() fails loudly and preserves the backup if restoring app.R fails", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  setup(app_dir)
  app_r <- file.path(app_dir, "app.R")
  backup <- paste0(app_r, ".shinycov_backup")
  expect_true(file.exists(backup))

  # Make the restore target read-only so file.copy() fails without
  # throwing (it returns FALSE instead).
  Sys.chmod(app_r, mode = "0444")
  on.exit(Sys.chmod(app_r, mode = "0644"), add = TRUE)

  # file.copy() itself emits a low-level "Permission denied" warning when
  # the underlying OS call fails -- expected here, not part of what this
  # test is checking, so suppress it rather than let it inflate the run's
  # warning tally.
  expect_error(suppressWarnings(cleanup(app_dir)), "Failed to restore")
  # The backup must survive a failed restore, not be deleted alongside a
  # false "success" message.
  expect_true(file.exists(backup))

  Sys.chmod(app_r, mode = "0644")
  cleanup(app_dir) # clean up for real now that the target is writable again
})
