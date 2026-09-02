#' Setup coverage instrumentation for a Shiny app
#'
#' Prepares a Shiny app directory for coverage collection:
#'
#' 1. Writes the bootstrap script to `.shiny.cov/bootstrap.R`.
#' 2. Creates an `app.R` wrapper that sources the bootstrap before the
#'    original app code, routing file loading through the bootstrap's
#'    `source_instrumented()` function which instruments code on-the-fly.
#' 3. Sets `SHINYCOV_OUTPUT` and `R_COVR=true` environment variables.
#'    (`R_COVR` is kept for compatibility with older shiny.cov releases;
#'    instrumentation keys off `SHINYCOV_OUTPUT` alone.)
#'
#' @param app_dir Path to the Shiny app directory. Defaults to `"."`.
#' @param output_dir Directory for coverage output. Defaults to
#'   `.shiny.cov/` inside `app_dir`.
#' @param overwrite_rprofile If `TRUE`, silently overwrite an existing
#'   `.Rprofile`. Default is `FALSE`. Kept for backwards compatibility.
#'
#' @return Invisibly returns the path to the output directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shiny.cov::setup("my-shiny-app")
#' shinytest2::test_app("my-shiny-app")
#' cov <- shiny.cov::collect("my-shiny-app")
#' covr::report(cov)
#' shiny.cov::cleanup("my-shiny-app")
#' }
setup <- function(
  app_dir = ".",
  output_dir = NULL,
  overwrite_rprofile = FALSE
) {
  app_dir_raw <- app_dir
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  if (!dir.exists(app_dir)) {
    stop("App directory does not exist: ", app_dir)
  }

  # normalizePath() follows symlinks with no check by design: setup() is
  # dev/CI tooling operating on a directory the caller already has full
  # write access to, not crossing a security boundary. Still, a symlinked
  # app_dir means setup()'s writes (app.R wrapper, .Rprofile, .shiny.cov/)
  # land at the resolved target rather than the path the caller passed in
  # -- worth a cheap heads-up. Only checks app_dir_raw itself, not an
  # intermediate path component -- deliberately cheap, not exhaustive.
  link_target <- Sys.readlink(app_dir_raw)
  if (!is.na(link_target) && nzchar(link_target)) {
    message(
      "Note: ", app_dir_raw, " is a symlink; shiny.cov will operate on ",
      "its resolved target, ", app_dir, "."
    )
  }

  # A leftover *.shinycov_backup means a previous setup() was interrupted
  # (crashed, killed CI job) between renaming the original file and
  # finishing the wrapper -- create_app_wrapper() only guards against
  # double-wrapping when *both* the backup and a wrapped app.R already
  # exist; if the process died in between, app_dir can be left with only
  # the backup and no usable app.R at all, and a naive re-run would either
  # silently do nothing useful (app.R missing entirely -- see
  # create_app_wrapper()'s early-return path in that case) or, if the
  # interruption happened at a different point, double-wrap on top of an
  # already-wrapped file. Refuse clearly instead of guessing which case
  # this is; cleanup() already knows how to unwind a backup unconditionally.
  leftover_backups <- list.files(
    app_dir,
    pattern = "\\.shinycov_backup$",
    full.names = TRUE
  )
  if (length(leftover_backups) > 0) {
    stop(
      "shiny.cov: found leftover backup file(s) from a previous, ",
      "apparently-interrupted setup() call:\n",
      paste("  -", leftover_backups, collapse = "\n"), "\n",
      "Run shiny.cov::cleanup(\"", app_dir, "\") first to restore the ",
      "original files, then call setup() again."
    )
  }

  # The generated bootstrap calls shiny.cov:::instrument_file() directly
  # instead of reimplementing it, so the child process (spawned by
  # shinytest2's callr::r_bg() or by a plain Rscript for Cypress) must be
  # able to load an *installed* copy of this package. A
  # pkgload::load_all() dev session doesn't count -- warn early rather
  # than letting the failure surface as silently-missing coverage inside
  # the child process later.
  if (!nzchar(system.file(package = "shiny.cov"))) {
    warning(
      "shiny.cov does not appear to be installed (system.file() found ",
      "nothing). The Shiny process spawned during testing must be able to ",
      "run requireNamespace(\"shiny.cov\") -- coverage will silently be ",
      "empty if it can't. If you're developing shiny.cov itself via ",
      "pkgload::load_all(), install a local copy first, e.g. ",
      "install.packages('.', repos = NULL, type = 'source')."
    )
  }

  # Fail fast with a clear message rather than an obscure error deep
  # inside the child process's bootstrap if {shiny} is missing or too old
  # to have the shinyApp() hook point this package relies on.
  if (!requireNamespace("shiny", quietly = TRUE) || !is.function(shiny::shinyApp)) {
    stop("shiny.cov requires the {shiny} package to be installed.")
  }
  shiny_version <- utils::packageVersion("shiny")
  if (shiny_version < "1.7.0") {
    stop("shiny.cov requires shiny >= 1.7.0 (found ", shiny_version, ").")
  }

  # ---- Output directory ----
  if (is.null(output_dir)) {
    output_dir <- shinycov_output_dir(app_dir)
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  # ---- 1. Write bootstrap ----
  bootstrap_path <- file.path(output_dir, "bootstrap.R")
  write_bootstrap(bootstrap_path)
  message("Wrote bootstrap to ", bootstrap_path)

  # ---- 2. Create app wrapper ----
  app_r_path <- file.path(app_dir, "app.R")
  has_app_r <- FALSE
  if (file.exists(app_r_path)) {
    first_line <- tryCatch(
      readLines(app_r_path, n = 1, warn = FALSE),
      error = function(e) character(0)
    )
    # readLines(n = 1) on a genuinely 0-byte app.R returns character(0),
    # not "" -- guard that explicitly so has_app_r is always a real
    # TRUE/FALSE (grepl() on character(0) returns logical(0), which would
    # otherwise propagate into `if (has_app_r)` as NA). An empty app.R is
    # treated as "not already a shiny.cov wrapper" and gets wrapped like
    # any other plain app.R, but flagged with a warning below since a
    # genuinely empty entry point is often a mistake.
    is_wrapper <- length(first_line) > 0 &&
      grepl("shiny.cov app wrapper", first_line, fixed = TRUE)
    has_app_r <- !is_wrapper
    if (length(first_line) == 0) {
      warning(
        "app.R at ", app_r_path, " is empty (0 bytes); wrapping it as-is. ",
        "If this wasn't intentional, check that your app's entry point ",
        "was written correctly.",
        call. = FALSE
      )
    }
  }

  ui_r_path <- file.path(app_dir, "ui.R")
  server_r_path <- file.path(app_dir, "server.R")
  has_ui_server <- !has_app_r &&
    file.exists(ui_r_path) &&
    file.exists(server_r_path)

  if (has_app_r) {
    create_app_wrapper(app_dir, bootstrap_path, "app.R")
  } else if (has_ui_server) {
    create_app_wrapper_ui_server(app_dir, bootstrap_path)
    has_app_r <- TRUE # app.R was just created
  } else {
    warning("No app.R or ui.R+server.R found in ", app_dir)
  }

  # ---- 3. .Rprofile (for non-callr starts like Cypress) ----
  if (has_app_r) {
    inject_rprofile(app_dir, bootstrap_path, overwrite_rprofile)
  }

  # ---- 4. Environment variables ----
  coverage_rds <- normalizePath(
    file.path(output_dir, "coverage.rds"),
    winslash = "/",
    mustWork = FALSE
  )
  Sys.setenv(SHINYCOV_OUTPUT = coverage_rds, R_COVR = "true")
  message("SHINYCOV_OUTPUT=", coverage_rds, " R_COVR=true")

  .shinycov_env$app_dir <- app_dir

  # Set interaction log path for shinytest2 adapter
  interaction_log_path <- file.path(output_dir, "interactions.json")
  shinycov_set_interaction_log(interaction_log_path)

  message(
    "shiny.cov setup complete. Run your tests, then shiny.cov::collect()."
  )
  invisible(output_dir)
}

# ---- Wrapper creation ----

#' Create an app.R wrapper that loads the bootstrap first
#'
#' Renames the original `app.R` to `<name>.shinycov_backup` and writes
#' a new `app.R` that:
#' 1. Sources the bootstrap (which swaps counters, defines `instrument_file`
#'    and `source_instrumented`)
#' 2. Calls `source_instrumented()` on the backup to run the original app
#'    code through the instrumentation pipeline
#'
#' @param app_dir Path to the app directory.
#' @param bootstrap_path Absolute path to `bootstrap.R`.
#' @param app_file Name of the app entry point (usually `"app.R"`).
#' @keywords internal
create_app_wrapper <- function(app_dir, bootstrap_path, app_file = "app.R") {
  app_r <- file.path(app_dir, app_file)
  backup <- paste0(app_r, ".shinycov_backup")
  bs_path <- normalizePath(bootstrap_path, winslash = "/")

  if (file.exists(backup)) {
    message("Wrapper backup already exists at ", backup)
    return(invisible())
  }

  file.rename(app_r, backup)

  # Ensure working directory is the app directory before sourcing
  # (needed so relative source() calls like source("R/helpers.R") work)
  #
  # Step 2 (running the backup) must work whether or not step 1 actually
  # defined source_instrumented() -- e.g. `Rscript app.R` run directly,
  # without SHINYCOV_OUTPUT set, skips loading the bootstrap in step 1
  # entirely, and the app must still run normally, uninstrumented.
  wrapper <- c(
    "# shiny.cov app wrapper -- auto-generated, do not edit",
    "# Restored by shiny.cov::cleanup()",
    "",
    "# Ensure working directory is the app directory",
    sprintf(
      'setwd(%s)',
      encodeString(normalizePath(app_dir, winslash = "/"), quote = '"')
    ),
    "",
    "# 1. Load the bootstrap (swaps counters, defines source_instrumented)",
    "#    only if coverage was actually requested for this run.",
    sprintf('if (nzchar(Sys.getenv("SHINYCOV_OUTPUT", ""))) {'),
    sprintf(
      '  tryCatch(source(%s), error = function(e) warning(e))',
      encodeString(bs_path, quote = '"')
    ),
    '}',
    "",
    "# 2. Run the original app code -- instrumented if step 1 defined",
    "#    source_instrumented(), otherwise run normally so the app still",
    "#    works when launched without coverage instrumentation requested.",
    sprintf('if (exists("source_instrumented", mode = "function")) {'),
    sprintf(
      '  source_instrumented(%s, envir = environment())',
      encodeString(normalizePath(backup, winslash = "/"), quote = '"')
    ),
    '} else {',
    sprintf(
      '  source(%s, local = environment())$value',
      encodeString(normalizePath(backup, winslash = "/"), quote = '"')
    ),
    '}'
  )

  writeLines(wrapper, app_r)
  message("Created app.R wrapper (original saved as ", basename(backup), ")")
  invisible()
}

#' Create an app.R wrapper for ui.R + server.R apps
#'
#' Writes a new `app.R` that sources the bootstrap, then uses
#' `source_instrumented()` to load `ui.R` and `server.R`, and
#' finally calls `shiny::shinyApp(ui, server)`.
#'
#' @param app_dir Path to the app directory.
#' @param bootstrap_path Absolute path to `bootstrap.R`.
#' @keywords internal
create_app_wrapper_ui_server <- function(app_dir, bootstrap_path) {
  app_r <- file.path(app_dir, "app.R")
  if (file.exists(app_r)) {
    return(invisible())
  }

  bs_path <- normalizePath(bootstrap_path, winslash = "/")

  wrapper <- c(
    "# shiny.cov app wrapper -- auto-generated, do not edit",
    "# Restored by shiny.cov::cleanup()",
    "",
    sprintf('if (nzchar(Sys.getenv("SHINYCOV_OUTPUT", ""))) {'),
    sprintf(
      '  tryCatch(source(%s), error = function(e) warning(e))',
      encodeString(bs_path, quote = '"')
    ),
    '}',
    "",
    "# Instrumented if the bootstrap above defined source_instrumented(),",
    "# otherwise run normally (uninstrumented) so the app still works when",
    "# launched without coverage instrumentation requested -- see the",
    "# matching comment in create_app_wrapper() for why this can't be",
    "# unconditional.",
    'if (exists("source_instrumented", mode = "function")) {',
    '  source_instrumented("ui.R", envir = environment())',
    '  source_instrumented("server.R", envir = environment())',
    '} else {',
    '  source("ui.R", local = environment())',
    '  source("server.R", local = environment())',
    '}',
    '',
    'shiny::shinyApp(ui = ui, server = server)'
  )

  writeLines(wrapper, app_r)
  message("Created app.R wrapper for ui.R + server.R")
  invisible()
}

# ---- .Rprofile injection (fallback for shell-based starts) ----

inject_rprofile <- function(app_dir, bootstrap_path, overwrite_rprofile) {
  rprofile_path <- app_rprofile_path(app_dir)

  snippet <- c(
    "",
    "# <<< shiny.cov coverage instrumentation >>>",
    "# Managed by shiny.cov::setup() / shiny.cov::cleanup(). Do not edit.",
    "if (nzchar(Sys.getenv(\"SHINYCOV_OUTPUT\", \"\"))) {",
    sprintf(
      "  tryCatch(source(%s),",
      encodeString(normalizePath(bootstrap_path, winslash = "/"), quote = '"')
    ),
    "           error = function(e) {",
    "             warning(\"shiny.cov bootstrap failed: \", conditionMessage(e))",
    "           })",
    "}",
    "# <<< end shiny.cov >>>",
    ""
  )

  if (file.exists(rprofile_path)) {
    existing <- readLines(rprofile_path, warn = FALSE)
    if (
      any(grepl("shiny.cov coverage instrumentation", existing, fixed = TRUE))
    ) {
      return(invisible())
    }
    # Always back up the existing .Rprofile before touching it, regardless
    # of overwrite_rprofile -- cleanup()'s restore_rprofile() looks for
    # this backup first and, if found, just copies it straight back. That
    # makes cleanup() correct for both branches below for free: it doesn't
    # need to know or care whether setup() appended or overwrote.
    backup_path <- rprofile_backup_path(app_dir)
    file.copy(rprofile_path, backup_path, overwrite = TRUE)
    if (overwrite_rprofile) {
      # Documented behavior: "silently overwrite an existing .Rprofile".
      # Replace the file's contents entirely with the shiny.cov snippet
      # rather than appending to whatever was there -- the backup taken
      # above is what makes this safe/reversible, not skipping it.
      writeLines(snippet, rprofile_path)
    } else {
      message(
        ".Rprofile exists; appending snippet (backup at ",
        backup_path,
        ")"
      )
      cat(paste(snippet, collapse = "\n"), file = rprofile_path, append = TRUE)
    }
  } else {
    writeLines(snippet, rprofile_path)
    message("Created ", rprofile_path)
  }
  invisible()
}
