# {box}-module instrumentation hook
#
# {box}-based apps (including rhino apps, whose root app.R is just
# rhino::app()) load every module file through box:::load_from_source(),
# not base::source() -- the source() override in bootstrap.R never sees
# this code at all. install_box_hook() patches load_from_source() itself
# so every box module gets the same shinycov_trace_calls() branch-level
# instrumentation as source()d files, writing into the same
# permanently-active counter environment (no separate merge step needed
# here, unlike instrument_file(): count() calls embedded in the
# instrumented expressions resolve the active counters at call time,
# which is already the process-wide counters bootstrap.R sets up).
#
# This lives here as a real, directly testable function
# (shiny.cov:::install_box_hook()) rather than as inline text inside
# inst/bootstrap/bootstrap.R (which otherwise delegates to the installed
# package; see that file's header). The box-module patch is the one piece
# of bootstrap logic that reaches into another package's unexported,
# unversioned internals, so it needs direct test coverage -- not tests
# hand-duplicating a copy of the logic instead of exercising what actually
# ships.
#
# box:::load_from_source() is internal, unexported {box} API with no
# stability guarantee across versions. Two failure modes matter here and
# get explicit guards below rather than being allowed to fail silently:
#   1. The function/binding itself might not exist (or might not resolve
#      to a function) in some future/past {box} version -- `$` on an
#      environment returns NULL for a missing name, not an error, so
#      proceeding without checking would build a broken stub from NULL and
#      *install* it in place of box's real loader, breaking box::use() for
#      the entire app with nothing but R warnings (never caught by a
#      tryCatch(error = ...)) to show for it.
#   2. The function might exist but have a different internal shape (the
#      `eval(exprs, mod_ns)` statement this patch surgically replaces
#      might move, change, or disappear) -- overwriting body(...)[[3]]
#      blindly in that case would replace the wrong statement.
# Both guards below make install_box_hook() degrade gracefully (skip
# instrumentation, leave load_from_source() untouched, message() why) in
# either case, so box::use() itself keeps working -- just without
# coverage tracking for module files -- instead of the app silently
# breaking or being left with a corrupted loader.

#' Patch \{box\}'s internal `load_from_source()` for branch-level coverage
#'
#' Installs a patched `box:::load_from_source()` that routes every box
#' module's top-level expressions through [shinycov_trace_calls()] before
#' evaluating them -- the same branch-level instrumentation
#' `instrument_file()` applies to plain `source()`d files -- so `covr`
#' coverage extends into `\{box\}`/rhino module files, not just the app's own
#' top-level `source()`d code.
#'
#' Reads `box`'s and `shiny.cov`'s own state directly; takes no arguments.
#' Meant to be called once per child app process, from
#' `inst/bootstrap/bootstrap.R`.
#'
#' Defensive by design: `box:::load_from_source()` is unexported, unversioned
#' internal API. If it isn't found as a function, or its body doesn't match
#' the exact shape this patch depends on, instrumentation is skipped (with
#' an explanatory `message()`) rather than installing a broken patch or
#' silently corrupting module loading -- `box::use()` keeps working either
#' way, just without coverage tracking for module files when skipped.
#'
#' @return Invisibly, `TRUE` if the hook was installed, `FALSE` if
#'   instrumentation was skipped for any of the reasons above (a `message()`
#'   explains which). Any other, genuinely unexpected error during
#'   installation is caught and also reported via `message()`, returning
#'   `FALSE` rather than propagating.
#' @keywords internal
install_box_hook <- function() {
  tryCatch({
    box_ns <- asNamespace("box")
    orig_load_from_source <- box_ns$load_from_source

    if (!is.function(orig_load_from_source)) {
      message(
        "shiny.cov: box:::load_from_source() was not found as a function ",
        "in the installed {box} package (version ",
        tryCatch(as.character(utils::packageVersion("box")), error = function(e) "unknown"),
        "). {box} module instrumentation is being skipped -- box::use() ",
        "will still work normally, just without branch-level coverage ",
        "tracking for module files."
      )
      return(invisible(FALSE))
    }

    box_eval_instrumented <- function(exprs, mod_ns) {
      top_srcrefs <- attr(exprs, "srcref")
      # box's own load_from_source() parses info$source_path as-is, which
      # on Windows is a native backslash path -- instrument_file() (used
      # for plain source()d files) always normalizes to forward slashes.
      # Left inconsistent, covr::report()'s HTML source browser silently
      # fails to match a file's entry in the file list to its embedded
      # source content for whichever files came through this path (all
      # top-level srcrefs of one file share a single mutable srcfile
      # environment, so fixing it once here fixes every srcref, including
      # ones trace_calls()/impute_srcref() create later for branches).
      if (length(top_srcrefs) > 0) {
        sf <- attr(top_srcrefs[[1]], "srcfile")
        if (!is.null(sf) && !is.null(sf$filename)) {
          sf$filename <- normalizePath(sf$filename, winslash = "/", mustWork = FALSE)
        }
      }
      instrumented <- lapply(seq_along(exprs), function(i) {
        e <- exprs[[i]]
        sr <- if (!is.null(top_srcrefs)) top_srcrefs[[i]] else NULL
        if (is.null(sr)) return(e)
        shinycov_trace_calls(e, parent_ref = sr)
      })
      for (e in instrumented) eval(e, mod_ns)
    }

    new_load_from_source <- orig_load_from_source

    # Surgically replace just the `eval(exprs, mod_ns)` statement (the
    # function body's 3rd element -- `{`, assignment, eval(), ...) rather
    # than reimplementing the rest of load_from_source() (export scanning,
    # legacy detection, S3 method registration) by hand -- keeps this
    # working across box versions as long as that function's overall shape
    # doesn't change. Check the shape here at runtime, every time, since the
    # box version actually installed in any given user's environment can
    # differ from the one this statement's position was confirmed against --
    # if it doesn't match, replacing it blindly would overwrite the wrong
    # statement and could silently produce broken/incomplete module loading
    # with no error either.
    expected_stmt <- quote(eval(exprs, mod_ns))
    actual_stmt <- tryCatch(body(new_load_from_source)[[3]], error = function(e) NULL)
    if (!identical(actual_stmt, expected_stmt)) {
      message(
        "shiny.cov: box:::load_from_source()'s body did not match the ",
        "expected shape (the 3rd top-level statement should be ",
        "`eval(exprs, mod_ns)`; found `",
        if (is.null(actual_stmt)) "NULL" else paste(deparse(actual_stmt), collapse = " "),
        "` instead). {box} module instrumentation is being skipped -- ",
        "box::use() will still work normally, just without branch-level ",
        "coverage tracking for module files."
      )
      return(invisible(FALSE))
    }
    body(new_load_from_source)[[3]] <- quote(box_eval_instrumented(exprs, mod_ns))

    # box_ns is a locked namespace environment; give the patched function
    # its own child environment (so it still resolves box's other internal
    # symbols like namespace_info/parse_export_specs/make_S3_methods_known
    # via lexical scoping) that also holds our helper.
    box_hook_env <- new.env(parent = box_ns)
    box_hook_env$box_eval_instrumented <- box_eval_instrumented
    environment(new_load_from_source) <- box_hook_env

    rlang::env_binding_unlock(box_ns, "load_from_source")
    assign("load_from_source", new_load_from_source, envir = box_ns)
    rlang::env_binding_lock(box_ns, "load_from_source")

    invisible(TRUE)
  }, error = function(e) {
    message("shiny.cov: could not instrument {box} module loading: ", conditionMessage(e))
    invisible(FALSE)
  })
}
