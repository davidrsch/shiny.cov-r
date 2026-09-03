# shinytest2 adapter
#
# Wraps shinytest2's AppDriver to automatically log UI interactions
# for shiny.cov's UI interaction coverage report.

.shinycov_interaction_log <- NULL

shinycov_set_interaction_log <- function(path) {
  .shinycov_env$interaction_log_path <- path
}
shinycov_get_interaction_log <- function() {
  .shinycov_env$interaction_log_path
}

log_interaction <- function(selector, action, value = NULL) {
  # Set by wait_for_value() while it's polling via repeated self$get_value()
  # calls, so a single wait_for_value() logs one entry instead of one per
  # poll iteration -- see wait_for_value() below for where this is set/reset.
  #
  # Accepted limitation: `suppress_log` lives in the process-global
  # `.shinycov_env`, not as a private field on the polling AppDriver
  # instance. Fine for the supported usage pattern -- one AppDriver
  # actively driving the browser per R process -- but not reentrant-safe if
  # two AppDrivers' wait_for_value() calls were ever interleaved on the
  # same process; documented rather than fixed, since nothing in this
  # package's design or tests exercises that scenario.
  if (isTRUE(.shinycov_env$suppress_log)) return(invisible())

  log_path <- shinycov_get_interaction_log()
  if (is.null(log_path)) return(invisible())

  entry <- list(
    selector = selector, action = action, value = value,
    timestamp = as.numeric(Sys.time())
  )

  log <- if (file.exists(log_path)) {
    tryCatch(jsonlite::fromJSON(log_path, simplifyVector = FALSE),
             error = function(e) list())
  } else list()

  log <- c(log, list(entry))
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

  # Atomic write: mirrors bootstrap.R's save_coverage() (see its comments
  # for the full justification, which applies identically here). Without
  # this, two concurrent AppDriver sessions logging against the same
  # app_dir race on a plain read-merge-overwrite cycle and can lose each
  # other's entries (classic lost update), and a process killed mid-write
  # (the periodic-save/SIGKILL scenario this project explicitly designs
  # around) can leave a truncated/corrupt interactions.json that the next
  # read's tryCatch() above would silently treat as an empty log, discarding
  # everything logged before the crash. Writing to a per-process tmp file
  # and rename()-ing into place (atomic on the same filesystem) narrows --
  # does not eliminate, a third writer landing between our read and rename
  # could still be missed -- this race, the same accepted trade-off already
  # used for coverage.rds rather than introducing real cross-process locking.
  tmp <- paste0(log_path, ".tmp", Sys.getpid())
  jsonlite::write_json(log, tmp, auto_unbox = TRUE, pretty = TRUE)
  file.rename(tmp, log_path)
  invisible()
}

#' Shiny.cov-aware AppDriver
#'
#' Wraps [shinytest2::AppDriver] to automatically log UI interactions for
#' shiny.cov's UI interaction coverage report. Logged: input mutations
#' (`set_inputs()`, `click()`, `upload_file()`), output/value reads
#' (`get_text()`, `get_html()`, `get_value()`, `get_values()`,
#' `expect_values()`, `wait_for_value()`), and window resizing
#' (`set_window_size()`). `run_js()` is also logged, but only as a single
#' non-attributable `"window"`/`"run_js"` entry for raw-log visibility --
#' see its own `@description` for why it can't be attributed to a specific
#' input/output. Any other `shinytest2::AppDriver` method not listed above
#' (e.g. navigating pages, taking screenshots) is *not* logged and so is
#' invisible to the coverage report. All methods below forward their
#' arguments to the corresponding `shinytest2::AppDriver` method; see that
#' class's documentation for full argument semantics. Logging only happens
#' after the underlying `shinytest2::AppDriver` call returns successfully --
#' if it throws, nothing is logged.
#'
#' **UI manifest limitation**: separately from interaction logging above,
#' the *manifest* of which UI elements exist at all (used to compute the
#' denominator of the coverage percentage) is only snapshotted twice by
#' default -- once in `initialize()`, once in `stop()`. Transient UI that
#' mounts and unmounts entirely within a test (a modal opened and closed,
#' a wizard step visited and left, a `conditionalPanel()`/`insertUI()`
#' element removed again) can be invisible at both snapshot points and so
#' silently excluded from the manifest -- not marked uncovered, just
#' absent, which inflates the apparent completeness of the reported
#' percentage. Call `discover_manifest()` yourself, mid-test, right after
#' revealing such elements and before they're removed, to capture them;
#' see its own `@description` for details.
#'
#' @importFrom R6 R6Class
#' @export
AppDriver <- R6::R6Class(
  "shinycovAppDriver",
  inherit = shinytest2::AppDriver,
  cloneable = FALSE,

  public = list(
    #' @description Create a new app driver, taking an initial UI manifest
    #'   snapshot right after the app loads. See `discover_manifest()` for
    #'   why an early snapshot matters, not just the one taken at `stop()`.
    #' @param ... Passed to `shinytest2::AppDriver$initialize()`.
    initialize = function(...) {
      # Tag this run's coverage by source ("shinytest2") so collect() can
      # report per-source counts. The child app process launched by
      # shinytest2 inherits this env var at spawn time; restore the parent's
      # previous value once the app is up.
      old_source <- Sys.getenv("SHINYCOV_SOURCE", unset = "")
      Sys.setenv(SHINYCOV_SOURCE = "shinytest2")
      on.exit({
        if (nzchar(old_source)) {
          Sys.setenv(SHINYCOV_SOURCE = old_source)
        } else {
          Sys.unsetenv("SHINYCOV_SOURCE")
        }
      }, add = TRUE)
      super$initialize(...)
      tryCatch(self$discover_manifest(), error = function(e) {
        message("shiny.cov: initial UI discovery failed: ", conditionMessage(e))
      })
    },

    #' @description Set one or more input values, logging each for the
    #'   UI interaction coverage report.
    #' @param ... Named input values, e.g. `set_inputs(bins = 20)`.
    #' @param wait_ Passed to `shinytest2::AppDriver$set_inputs()`.
    set_inputs = function(..., wait_ = TRUE) {
      # `values_` is deliberately not forwarded to super$set_inputs():
      # shinytest2::AppDriver$set_inputs() has no such formal argument, so
      # it would be swept into super's own `...` and misinterpreted as a
      # real input literally named "values_", then fail with "Unable to
      # find input binding for element with id values_". Forward only
      # what the real method accepts.
      #
      # Use rlang::list2(), not base::list(), to build `inputs`: shinytest2's
      # own click(input = "x") is implemented internally as
      # self$set_inputs(!!x := "click", ...), rlang's dynamic-dots syntax.
      # Since R6 method dispatch on `self$` always resolves to the
      # most-derived override (this one), even when called from within
      # shinytest2's own click() implementation, a plain list(...) here
      # doesn't understand `:=`/`!!` and errors with "`:=` can only be used
      # within dynamic dots."
      inputs <- rlang::list2(...)
      # Log only after super$set_inputs() actually succeeds -- a thrown
      # error (bad id, timeout, ...) must not leave a phantom "interacted"
      # entry behind. super$set_inputs() always returns invisible(self)
      # so wrap explicitly to preserve that.
      result <- super$set_inputs(..., wait_ = wait_)
      for (name in names(inputs)) {
        log_interaction(paste0("#", name), "set_inputs", inputs[[name]])
      }
      invisible(result)
    },

    #' @description Click an input, an output, or a CSS selector, logging
    #'   the click.
    #' @param input Id of an input to click (e.g. an `actionButton`).
    #' @param output Id of an output to click (e.g. an interactive plot),
    #'   if not using `input`.
    #' @param selector CSS selector to click, if not using `input` or
    #'   `output`.
    #' @param ... Passed to `shinytest2::AppDriver$click()`.
    click = function(input = rlang::missing_arg(), output = rlang::missing_arg(),
                      selector = rlang::missing_arg(), ...) {
      # Real shinytest2::AppDriver$click() signature is `function(input =
      # missing_arg(), output = missing_arg(), selector = missing_arg(),
      # ...)` -- THREE possible targets, not just input/selector. Forwarding
      # unconditionally with these same missing_arg() defaults (mirroring
      # get_values()'s pattern below) is equivalent to the caller omitting
      # whichever ones they didn't supply.
      target <- if (!rlang::is_missing(input)) {
        input
      } else if (!rlang::is_missing(output)) {
        output
      } else if (!rlang::is_missing(selector)) {
        selector
      } else {
        NULL
      }
      # Only massage `target` into a "#id" selector when it's a single,
      # non-NA string. Anything else (e.g. a length-2 vector) is left
      # untouched and falls through to super$click() below, which performs
      # its own validation and throws its own clear error message.
      if (!is.null(target) && is.character(target) && length(target) == 1 &&
          !is.na(target) && !grepl("^#", target)) {
        target <- paste0("#", target)
      }
      # super$click() always returns invisible(self); logging happens only
      # once it actually returns, i.e. only on success.
      result <- super$click(input = input, output = output, selector = selector, ...)
      log_interaction(target, "click")
      invisible(result)
    },

    #' @description Upload a file to a file input, logging the upload.
    #' @param ... A single named argument identifying the file input and the
    #'   file to upload, e.g. `upload_file(myFileInputId = "path/to/file")`
    #'   -- passed to `shinytest2::AppDriver$upload_file()`.
    #' @param wait_ Passed to `shinytest2::AppDriver$upload_file()`.
    #' @param timeout_ Passed to `shinytest2::AppDriver$upload_file()`.
    upload_file = function(..., wait_ = TRUE, timeout_ = rlang::missing_arg()) {
      # Real shinytest2::AppDriver$upload_file() signature is `function(...,
      # wait_ = TRUE, timeout_ = missing_arg())` -- ONE named dynamic-dots
      # argument identifying the file input (e.g.
      # `upload_file(myInputId = "path/to/file")`), not positional
      # selector/file arguments.
      #
      # Use rlang::list2(), not base::list(), to inspect `inputs` -- same
      # reasoning as set_inputs() above: shinytest2's internals build calls
      # using rlang's dynamic-dots syntax, and a plain list(...) doesn't
      # understand `:=`/`!!`.
      inputs <- rlang::list2(...)
      # super$upload_file() returns invisible(NULL); logging happens only
      # once it actually returns, i.e. only on success. If `inputs` isn't
      # exactly one named argument, super$upload_file() itself throws before
      # we get here, so nothing gets logged for a malformed call either.
      result <- super$upload_file(..., wait_ = wait_, timeout_ = timeout_)
      if (length(inputs) == 1 && rlang::is_named(inputs)) {
        log_interaction(paste0("#", names(inputs)[[1]]), "upload_file", inputs[[1]])
      }
      invisible(result)
    },

    # ---- Output verifications ----

    #' @description Read the text content of an output, logging the
    #'   verification.
    #' @param selector CSS selector of the output.
    #' @param ... Passed to `shinytest2::AppDriver$get_text()`.
    get_text = function(selector, ...) {
      # super$get_text() returns its result visibly, so no invisible() wrap
      # here.
      result <- super$get_text(selector, ...)
      log_interaction(selector, "get_text")
      result
    },

    #' @description Read the HTML content of an output, logging the
    #'   verification.
    #' @param selector CSS selector of the output.
    #' @param ... Passed to `shinytest2::AppDriver$get_html()`.
    get_html = function(selector, ...) {
      # super$get_html() returns its result visibly, so no invisible() wrap
      # here.
      result <- super$get_html(selector, ...)
      log_interaction(selector, "get_html")
      result
    },

    #' @description Read the current value of an input, output, or export,
    #'   logging the verification.
    #' @param ... Passed to `shinytest2::AppDriver$get_value()`.
    #' @param input,output,export Passed to
    #'   `shinytest2::AppDriver$get_value()`. Left at their real defaults
    #'   (rlang's `missing_arg()` sentinel, not `NULL`) when not supplied --
    #'   see the `input`/`output`/`export` note on `get_values()` below for
    #'   why. Exactly one of the three must actually be supplied;
    #'   `super$get_value()` itself enforces that and throws otherwise.
    #' @param hash_images Passed to `shinytest2::AppDriver$get_value()`.
    get_value = function(..., input = rlang::missing_arg(),
                          output = rlang::missing_arg(),
                          export = rlang::missing_arg(),
                          hash_images = FALSE) {
      # shinytest2::AppDriver$get_value()'s real signature is `function(...,
      # input = missing_arg(), output = missing_arg(), export =
      # missing_arg(), hash_images = FALSE)`, mirroring its sibling
      # get_values() below -- `output`/`export` are equally valid targets.
      #
      # Must forward as `input = input` (named), not a bare positional
      # `input` -- in base R, formals after `...` can only ever be matched
      # by exact name, never by position (see get_values()'s analogous note
      # below).
      #
      # super$get_value() returns its result visibly, so no invisible() wrap
      # here.
      result <- super$get_value(
        ..., input = input, output = output, export = export,
        hash_images = hash_images
      )
      # Mirrors get_values()'s per-target logging below: log a `get_value`
      # entry if `input` was the target, a `get_text` entry if `output` was,
      # and nothing if `export` was (exports aren't part of the UI manifest
      # coverage tracks -- see wait_for_value()'s doc comment for the same
      # reasoning).
      if (!rlang::is_missing(input)) {
        log_interaction(paste0("#", input), "get_value")
      } else if (!rlang::is_missing(output)) {
        log_interaction(paste0("#", output), "get_text")
      }
      result
    },

    #' @description Read all current input/output/export values at once
    #'   (`shinytest2::AppDriver$get_values()`), logging one `get_value`
    #'   entry per input id and one `get_text` entry per output id present
    #'   in the result.
    #' @param ... Passed to `shinytest2::AppDriver$get_values()`.
    #' @param input,output,export Passed to
    #'   `shinytest2::AppDriver$get_values()`. Left at their real defaults
    #'   (rlang's `missing_arg()` sentinel, not `NULL`) when not supplied --
    #'   these are dynamic-dots arguments in the underlying method, and
    #'   `get_values()`'s own "return everything" default behavior is
    #'   conditioned on detecting all three as genuinely missing, not as
    #'   `NULL`. Passing `NULL` here instead would silently turn "get
    #'   everything" into "get nothing".
    #' @param hash_images Passed to `shinytest2::AppDriver$get_values()`.
    get_values = function(..., input = rlang::missing_arg(),
                           output = rlang::missing_arg(),
                           export = rlang::missing_arg(),
                           hash_images = FALSE) {
      # super$get_values() returns its result (a list with $input/$output/
      # $export) visibly, so no invisible() wrap here.
      result <- super$get_values(
        ..., input = input, output = output, export = export,
        hash_images = hash_images
      )
      for (id in names(result$input)) {
        log_interaction(paste0("#", id), "get_value")
      }
      for (id in names(result$output)) {
        log_interaction(paste0("#", id), "get_text")
      }
      result
    },

    #' @description Snapshot-test all current input/output/export values
    #'   (`shinytest2::AppDriver$expect_values()`), logging one `get_value`
    #'   entry per input id and one `get_text` entry per output id present
    #'   in the snapshot.
    #' @param ... Passed to `shinytest2::AppDriver$expect_values()`.
    #' @param input,output,export,screenshot_args,name,transform Passed to
    #'   `shinytest2::AppDriver$expect_values()`. See the `input`/`output`/
    #'   `export` note on `get_values()` above for why `input`/`output`/
    #'   `export` default to `rlang::missing_arg()`, not `NULL`.
    expect_values = function(..., input = rlang::missing_arg(),
                              output = rlang::missing_arg(),
                              export = rlang::missing_arg(),
                              screenshot_args = rlang::missing_arg(),
                              name = NULL, transform = NULL) {
      # super$expect_values() returns invisible(content) where `content` is
      # the prettified JSON string it just snapshotted -- not the parsed
      # list get_values() returns. Parse it locally (a parse failure just
      # means no log entries, not an error) to find which input/output ids
      # were actually captured.
      result <- super$expect_values(
        ..., input = input, output = output, export = export,
        screenshot_args = screenshot_args, name = name, transform = transform
      )
      parsed <- tryCatch(
        jsonlite::fromJSON(result, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.null(parsed)) {
        for (id in names(parsed$input)) {
          log_interaction(paste0("#", id), "get_value")
        }
        for (id in names(parsed$output)) {
          log_interaction(paste0("#", id), "get_text")
        }
      }
      invisible(result)
    },

    #' @description Poll until an input/output/export value satisfies a
    #'   condition (`shinytest2::AppDriver$wait_for_value()`), logging one
    #'   entry for whichever specific input or output it targets.
    #'
    #'   `shinytest2::AppDriver$wait_for_value()`'s own implementation polls
    #'   by calling `self$get_value()` repeatedly (every `interval` ms,
    #'   R6's `self$` dispatch always resolving to *this* class's own
    #'   overrides) -- so without suppressing logging during that inner
    #'   polling loop, one
    #'   `wait_for_value()` call would log a duplicate entry per poll
    #'   iteration instead of the single entry documented here. Logging is
    #'   suppressed for the duration of the underlying call and a single
    #'   entry is logged afterward instead, based on whichever of
    #'   `input`/`output` was actually supplied. If `export` was the target
    #'   instead, nothing is logged -- there's no `export` entry in the
    #'   shared action vocabulary (`shinycov_input_actions()`/
    #'   `shinycov_output_actions()` in `R/utils.R`) to log it as, since
    #'   exports aren't part of the UI manifest coverage tracks.
    #' @param ... Passed to `shinytest2::AppDriver$wait_for_value()`.
    #' @param input,output,export Passed to
    #'   `shinytest2::AppDriver$wait_for_value()`. See the `input`/`output`/
    #'   `export` note on `get_values()` above for why these default to
    #'   `rlang::missing_arg()`, not `NULL`.
    #' @param ignore,timeout,interval Passed to
    #'   `shinytest2::AppDriver$wait_for_value()`.
    wait_for_value = function(..., input = rlang::missing_arg(),
                               output = rlang::missing_arg(),
                               export = rlang::missing_arg(),
                               ignore = list(NULL, ""),
                               timeout = rlang::missing_arg(),
                               interval = 400) {
      old_suppress <- isTRUE(.shinycov_env$suppress_log)
      .shinycov_env$suppress_log <- TRUE
      on.exit(.shinycov_env$suppress_log <- old_suppress, add = TRUE)

      # super$wait_for_value() returns its resolved value visibly -- no
      # invisible() wrap. If it errors (e.g. timeout reached), the error propagates normally
      # and nothing below runs, so nothing gets logged for a target that
      # was never actually reached.
      result <- super$wait_for_value(
        ..., input = input, output = output, export = export,
        ignore = ignore, timeout = timeout, interval = interval
      )

      .shinycov_env$suppress_log <- old_suppress
      if (!rlang::is_missing(input)) {
        log_interaction(paste0("#", input), "get_value")
      } else if (!rlang::is_missing(output)) {
        log_interaction(paste0("#", output), "get_text")
      }
      result
    },

    # ---- Other ----

    #' @description Resize the browser window, logging the change.
    #' @param width New window width in pixels.
    #' @param height New window height in pixels.
    set_window_size = function(width, height) {
      # super$set_window_size() always returns invisible(self).
      result <- super$set_window_size(width, height)
      log_interaction("window", "set_window_size",
                      list(width = width, height = height))
      invisible(result)
    },

    #' @description Run arbitrary JavaScript in the app's browser session
    #'   (`shinytest2::AppDriver$run_js()`), e.g. tests that set inputs via
    #'   `Shiny.setInputValue(...)` directly instead of `set_inputs()`.
    #'   Logs a single non-attributable entry (selector `"window"`, action
    #'   `"run_js"`) for raw-log visibility only -- deliberately does *not*
    #'   attempt to regex-parse `script` to guess which input/output it
    #'   might target, and `"run_js"` is intentionally *not* included in
    #'   `shinycov_input_actions()`/`shinycov_output_actions()`
    #'   (`R/utils.R`), so this entry is never counted as a "hit" on any UI
    #'   element by `merge_ui_coverage()`. Attributing an arbitrary JS
    #'   string to a specific element would be guessing, not measuring.
    #' @param script,...,file,timeout Passed to
    #'   `shinytest2::AppDriver$run_js()`.
    run_js = function(script = rlang::missing_arg(), ...,
                       file = rlang::missing_arg(),
                       timeout = rlang::missing_arg()) {
      # super$run_js() always returns invisible(self).
      result <- super$run_js(script = script, ..., file = file, timeout = timeout)
      log_interaction(
        "window", "run_js",
        if (!rlang::is_missing(script)) script else NULL
      )
      invisible(result)
    },

    # ---- UI discovery ----

    #' @description Discover the live UI manifest (inputs, outputs, tabs,
    #'   conditional panels) by asking the browser's own
    #'   `Shiny.inputBindings`/`Shiny.outputBindings` registry what's
    #'   actually bound on the page -- not by guessing from CSS classes, so
    #'   this works for any widget library, not just base Shiny. Merges
    #'   with any previously-written manifest (see
    #'   [merge_manifest_snapshots()] for why: some widget libraries'
    #'   distinguishing class/attributes only exist right after render and
    #'   disappear once the widget's own JS reacts to an interaction, so a
    #'   single after-the-fact snapshot can undercount specificity).
    #'   Writes `.shiny.cov/manifest.json`. Called automatically on
    #'   `initialize()` and by `stop()` -- **but those are only two
    #'   snapshots in time**, so anything that mounts into the DOM and is
    #'   unmounted again before `stop()` runs (a `showModal()` dialog closed
    #'   before the test ends, a wizard step advanced past, a
    #'   `conditionalPanel()`/`insertUI()` element removed again) is bound
    #'   at neither snapshot point and so is silently absent from the
    #'   manifest entirely -- not counted as "uncovered", just excluded,
    #'   which makes the overall coverage percentage look more complete
    #'   than it really is. `discover_manifest()` is exposed publicly for
    #'   exactly this reason: call it yourself, mid-test, right after
    #'   interacting with any such transient UI and while it's still
    #'   mounted, e.g. immediately after `showModal()` and before whatever
    #'   later closes it. Its snapshots merge with prior ones (see above),
    #'   so calling it more often is always safe, never lossy.
    #' @return The discovered manifest, invisibly.
    discover_manifest = function() {
      manifest <- tryCatch(self$get_js(discover_bindings_js()), error = function(e) {
        message("shiny.cov: UI discovery failed: ", conditionMessage(e))
        NULL
      })
      if (is.null(manifest)) {
        return(invisible(NULL))
      }
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        out_dir <- shinycov_output_dir(self$get_dir())
        dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
        manifest_path <- file.path(out_dir, "manifest.json")
        if (file.exists(manifest_path)) {
          previous <- tryCatch(
            jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
            error = function(e) NULL
          )
          if (!is.null(previous)) {
            manifest <- merge_manifest_snapshots(previous, manifest)
          }
        }
        # Atomic write, same tmp-then-rename pattern and justification as
        # log_interaction() above (and bootstrap.R's save_coverage()): a
        # second concurrent AppDriver session discovering/writing the same
        # app_dir's manifest.json, or a process killed mid-write, must not
        # be able to leave manifest.json truncated/corrupt or silently lose
        # the other session's snapshot to a last-writer-wins overwrite.
        tmp <- paste0(manifest_path, ".tmp", Sys.getpid())
        writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE), tmp)
        file.rename(tmp, manifest_path)
      }
      invisible(manifest)
    },

    #' @description Stop the app, discovering the final UI manifest first
    #'   while the browser session is still alive to ask.
    #' @param ... Passed to `shinytest2::AppDriver$stop()`.
    stop = function(...) {
      tryCatch(self$discover_manifest(), error = function(e) {
        message("shiny.cov: UI discovery before stop() failed: ", conditionMessage(e))
      })
      super$stop(...)
    }
  )
)
