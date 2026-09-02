# AST instrumentation for R source files
#
# Parses R source files, walks their ASTs, and wraps each top-level
# expression with a counter-increment call, via the tracing engine in
# R/trace.R (ported from covr -- see that file's header for why it's a
# port rather than covr::: calls). The instrumented expressions, when
# evaluated, record which lines execute.
#
# instrument_file() gives each file its own isolated counter environment
# by swapping it into shinycov_set_counters() during instrumentation and
# restoring the previous one afterward -- needed so the package's own
# tests, which call instrument_file() many times in one R session, don't
# see each other's entries.

#' Instrument an R source file
#'
#' Parses a file with srcref attributes, wraps every top-level expression
#' with a counter-increment call, and returns the instrumented expressions
#' along with the counter environment.
#'
#' @param path Path to an R source file.
#' @return A list with components `exprs` (list of instrumented language
#'   objects) and `counters` (environment keyed by srcref keys).
#' @keywords internal
instrument_file <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # Parse with srcref attributes. R attaches one srcref per top-level
  # expression to the parsed expression list itself (not to the individual
  # elements) -- we look those up positionally and pass each one to
  # shinycov_trace_calls() as `parent_ref` so it can recurse into the
  # expression and instrument every branch/statement inside it (if/else,
  # nested calls, function bodies, ...), not just the top-level statement.
  # We deliberately do NOT reattach the srcref onto parsed[[i]] itself:
  # trace_calls() expects attr(x, "srcref") (when present) to be a list of
  # per-child srcrefs matching length(x), and a single whole-statement
  # srcref there breaks its internal Map(), since its length never matches
  # length(x).
  parsed <- tryCatch(
    parse(file = path, keep.source = TRUE),
    error = function(e) {
      stop("Failed to parse ", path, ": ", conditionMessage(e))
    }
  )

  top_srcrefs <- attr(parsed, "srcref")

  # Create a fresh counter environment for this file
  counter_env <- new.env(parent = emptyenv())

  # Swap in this file's isolated counters for the duration of tracing,
  # restoring whatever was active before on exit.
  old_counters <- shinycov_set_counters(counter_env)
  on.exit(shinycov_set_counters(old_counters))

  exprs <- lapply(seq_along(parsed), function(i) {
    e <- parsed[[i]]
    sr <- if (!is.null(top_srcrefs)) top_srcrefs[[i]] else NULL
    if (is.null(sr)) {
      return(e)
    }
    shinycov_trace_calls(e, parent_ref = sr)
  })

  list(exprs = exprs, counters = counter_env)
}

#' Instrument R code passed as a character vector
#'
#' Writes the code to a temporary file so that srcref attributes have
#' correct file paths, then calls [instrument_file()].
#'
#' @param code Character vector of R source lines.
#' @return A list with components `exprs` and `counters`.
#' @keywords internal
instrument_code <- function(code) {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(code, tmp)
  instrument_file(tmp)
}

#' Evaluate instrumented expressions in a given environment
#'
#' Temporarily swaps the counter environment into the active counters so
#' that increment calls write to the correct environment, evaluates
#' each expression in sequence, then restores the original counters.
#'
#' @param instrumented Result from [instrument_file()] or [instrument_code()].
#' @param env Environment in which to evaluate the expressions.
#' @return Invisibly returns the value of the last expression.
#' @keywords internal
eval_instrumented <- function(instrumented, env = parent.frame()) {
  old_counters <- shinycov_set_counters(instrumented$counters)
  on.exit(shinycov_set_counters(old_counters))

  result <- NULL
  for (e in instrumented$exprs) {
    result <- eval(e, envir = env)
  }
  invisible(result) # last expression value propagates to caller
}
