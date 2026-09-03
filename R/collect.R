shinycov_coverage_files <- function(app_dir) {
  out <- shinycov_output_dir(app_dir)
  if (!dir.exists(out)) return(character(0))
  files <- list.files(out, pattern = "^coverage.*\\.rds$", full.names = TRUE)
  files <- files[!grepl("\\.tmp[0-9]+$", files)]
  sort(files)
}

shinycov_source_from_file <- function(files) {
  vapply(files, function(f) {
    b <- basename(f)
    if (identical(b, "coverage.rds")) return("total")
    gsub("^coverage\\.|\\.rds$", "", b)
  }, character(1))
}

sum_counters <- function(by_source) {
  # Single source (the common case): pass through untouched, so malformed
  # entries survive for reconstruct_counters() to diagnose rather than being
  # silently coerced to a number.
  if (length(by_source) == 1) return(by_source[[1]])

  all_keys <- unique(unlist(lapply(by_source, names), use.names = FALSE))
  out <- list()
  for (k in all_keys) {
    entries <- Filter(Negate(is.null), lapply(by_source, `[[`, k))
    if (length(entries) == 0) next
    # Preserve malformed (non-list, non-numeric) entries as-is.
    if (any(!vapply(entries, function(e) is.list(e) || is.numeric(e), logical(1)))) {
      out[[k]] <- entries[[1]]
      next
    }
    ref <- entries[[1]]
    total <- sum(vapply(entries, function(e) {
      if (is.list(e)) e$value %||% 0L else as.numeric(e)
    }, numeric(1)))
    if (is.list(ref)) {
      ref$value <- total
      out[[k]] <- ref
    } else {
      out[[k]] <- total
    }
  }
  out
}

#' Collect coverage data from a Shiny app test run
#'
#' Reads the `coverage.rds` file written by the Shiny child process on
#' shutdown and converts it into a standard `covr` coverage object
#' compatible with `covr::report()`, `covr::codecov()`, etc.
#'
#' @param app_dir Path to the Shiny app directory. Must have been
#'   previously configured with [setup()]. Defaults to `"."`.
#' @param output_file Direct path to a specific `coverage.rds` file.
#'   Overrides `app_dir` if provided.
#'
#' @return A `coverage` object (class `c("coverage", "list")`)
#'   compatible with covr's reporting infrastructure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cov <- shiny.cov::collect("my-shiny-app")
#' covr::report(cov)
#' covr::percent_coverage(cov)
#' }
collect <- function(app_dir = ".", output_file = NULL) {
  if (is.null(output_file)) {
    files <- shinycov_coverage_files(app_dir)
    if (length(files) == 0) {
      stop(
        "Coverage file not found in ", shinycov_output_dir(app_dir),
        "\n",
        "  Have you run your tests after calling shiny.cov::setup()?\n",
        "  The Shiny process must shut down gracefully for coverage to be written."
      )
    }
    by_source <- lapply(files, function(f) {
      tryCatch(readRDS(f), error = function(e) {
        stop("Failed to read coverage file: ", f, " (", conditionMessage(e), ")")
      })
    })
    names(by_source) <- shinycov_source_from_file(files)
    counters <- sum_counters(by_source)
    attr(counters, "shinycov_sources") <- by_source
  } else {
    if (!file.exists(output_file)) {
      stop("Coverage file not found at ", output_file)
    }
    counters <- tryCatch(readRDS(output_file), error = function(e) {
      stop("Failed to read coverage file: ", output_file, " (", conditionMessage(e), ")")
    })
  }

  if (length(counters) == 0) {
    warning("Coverage file is empty -- no code was instrumented or executed.")
    cov <- list()
    class(cov) <- c("coverage", "list")
    return(cov)
  }

  sources <- attr(counters, "shinycov_sources", exact = TRUE)
  if (is.null(sources)) {
    message("Read ", length(counters), " tracked expressions from ", output_file)
  } else {
    message(
      "Read ", length(counters), " tracked expressions across sources: ",
      paste(names(sources), collapse = ", ")
    )
  }

  counter_env <- reconstruct_counters(counters)
  cov <- build_coverage(counter_env)
  class(cov) <- c("coverage", class(cov))
  attr(cov, "package") <- NULL
  attr(cov, "type") <- "coverage"
  attr(cov, "shinycov_sources") <- sources

  # Merge UI interaction coverage directly into `cov` (see merge_ui_coverage()
  # for why this is done as additional entries rather than a separate
  # report): the goal is one number, not a server number and a UI number.
  cov <- merge_ui_coverage(cov, app_dir)

  pct <- if (length(cov) > 0) round(covr::percent_coverage(cov), 1) else 0
  message("Coverage: ", pct, "% (", length(cov), " tracked entries, R lines + UI elements combined)")
  cov
}

# ---- Internal helpers ----

#' Reconstruct a covr-compatible counter environment from a named list
#' @param counters Named list of counter entries from `readRDS()`.
#' @return An environment suitable for `covr:::as_coverage()`.
#' @keywords internal
reconstruct_counters <- function(counters) {
  env <- new.env(parent = emptyenv())
  skipped <- 0L
  for (key in names(counters)) {
    entry <- counters[[key]]
    if (is.list(entry)) {
      assign(key, entry, envir = env)
    } else if (is.numeric(entry)) {
      assign(key, list(value = entry), envir = env)
    } else {
      # An entry that's neither a list nor a numeric value can't be turned
      # into a counter (e.g. a corrupted coverage.rds, or one written by an
      # incompatible/older shiny.cov version) -- count it so the message
      # below can report it, rather than dropping it silently.
      skipped <- skipped + 1L
    }
  }
  if (skipped > 0L) {
    message(
      "shiny.cov: skipped ", skipped, " malformed counter entr",
      if (skipped == 1L) "y" else "ies",
      " in coverage.rds (neither a list nor a numeric value) -- this ",
      "usually means the file is corrupted or was written by an ",
      "incompatible/older version of shiny.cov. The coverage percentage ",
      "reported below excludes these entries and may be artificially low."
    )
  }
  env
}

#' Build a covr-compatible coverage object
#'
#' Produces a coverage list where each element represents one instrumented
#' expression with `$srcref` (8-element numeric srcref) and `$value`
#' (integer count). This matches the format `covr::report()`,
#' `covr::percent_coverage()`, and `covr::codecov()` expect.
#'
#' @param counter_env Environment with entries keyed by srcref strings,
#'   each containing `$value` (integer) and `$srcref` (srcref object).
#' @return A coverage list, one element per expression, compatible with
#'   all covr reporting functions.
#' @keywords internal
build_coverage <- function(counter_env) {
  keys <- ls(counter_env, all.names = TRUE)
  result <- list()
  skipped <- character(0)

  for (k in keys) {
    entry <- counter_env[[k]]
    sr <- entry$srcref
    if (is.null(sr)) {
      # An entry that's neither a list nor a numeric value can't be turned
      # into a counter (e.g. a corrupted coverage.rds, or one written by an
      # incompatible/older shiny.cov version) -- count it so the message
      # below can report it, rather than dropping it silently.
      skipped <- c(skipped, k)
      next
    }

    value <- entry$value %||% 0L

    # Keep srcref as an srcref object (not numeric).
    # covr's display_name() needs attr(sr, "srcfile") to get filenames.
    # as.data.frame.coverage() uses c(srcref, value) which works
    # because c() on an srcref extracts the 6 numeric components.
    result[[k]] <- list(
      srcref    = sr,
      value     = value,
      functions = entry$functions %||% character(0)
    )
  }

  if (length(skipped) > 0L) {
    shown <- utils::head(skipped, 5L)
    message(
      "shiny.cov: skipped ", length(skipped), " counter entr",
      if (length(skipped) == 1L) "y" else "ies",
      " with a missing/malformed srcref while building coverage (",
      paste(shown, collapse = ", "),
      if (length(skipped) > length(shown)) ", ..." else "",
      ") -- this usually means coverage.rds is corrupted or was written by ",
      "an incompatible/older version of shiny.cov. The coverage percentage ",
      "reported below excludes these entries and may be artificially low."
    )
  }

  result
}

#' Run coverage for a Shiny app tested with shinytest2
#'
#' One call: sets up instrumentation, runs tests, collects coverage.
#' Returns a covr-compatible coverage object ready for `covr::report()`.
#'
#' @param app_dir Path to the Shiny app directory. Default `"."`.
#' @param ...   Passed to `shinytest2::test_app()`.
#'
#' @return A `coverage` object.
#' @export
covr_r <- function(app_dir = ".", ...) {
  if (!requireNamespace("shinytest2", quietly = TRUE)) {
    stop("shinytest2 is required for covr_r(). Install with: install.packages('shinytest2')")
  }
  setup(app_dir)
  on.exit(tryCatch(cleanup(app_dir), error = function(e) NULL), add = TRUE)
  tryCatch(shinytest2::test_app(app_dir, ...), error = function(e) {
    warning("Tests failed: ", conditionMessage(e))
  })
  collect(app_dir)
}

# ---- UI-aware coverage adjustment ----

#' Escape a string for literal use inside a base R (POSIX extended) regex
#'
#' Used to embed a manifest-derived UI element id in a `grepl()` pattern
#' (see `locate_by_text()` inside `merge_ui_coverage()`) without any of its
#' characters being interpreted as regex syntax. Order matters: the
#' backslash itself is escaped first, before escaping any of the other
#' metacharacters below -- escaping in the other order would double-escape
#' the backslashes those steps introduce.
#'
#' @param x Character scalar.
#' @return `x` with every base R regex metacharacter (`. \\ | ( ) [ ] \{ \}
#'   ^ $ * + ?` -- the full extended-regex metacharacter set, see `?regex`)
#'   escaped, so it matches only as a literal string.
#' @keywords internal
regex_escape <- function(x) {
  metachars <- c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
  for (ch in metachars) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x
}

#' Find the line range of the smallest enclosing multi-line expression
#'
#' Used to make an untested UI element mark its *whole* widget expression
#' (e.g. `ComboBox.shinyInput(ns("kpi_years"), ...)` across several lines),
#' not just the single line the id literal sits on.
#'
#' @param lines Character vector of source lines.
#' @param ln A 1-based line number.
#' @return `integer(2)` `c(start_line, end_line)`; falls back to `c(ln, ln)`.
#' @keywords internal
enclosing_expr_range <- function(lines, ln) {
  pd <- tryCatch(
    utils::getParseData(parse(text = lines, keep.source = TRUE), includeText = TRUE),
    error = function(e) NULL
  )
  if (is.null(pd)) return(c(ln, ln))

  # Walk up from the string literal(s) on `ln` to the nearest function call
  # that isn't a namespacing wrapper (`ns`/`NS`). For apps that don't use
  # modules at all there is no wrapper, so the first enclosing call *is* the
  # widget and is returned directly. This resolves
  # `ComboBox.shinyInput(ns("kpi_years"), ...)` to the whole widget while a
  # single-line `uiOutput(ns("pcghge"))` stays a single line.
  str_toks <- pd[pd$token == "STR_CONST" & pd$line1 == ln, , drop = FALSE]
  if (nrow(str_toks) == 0) return(c(ln, ln))

  ranges <- list()
  for (i in seq_len(nrow(str_toks))) {
    cur <- str_toks$id[[i]]
    repeat {
      par <- pd$parent[pd$id == cur]
      if (length(par) == 0 || is.na(par[[1]]) || par[[1]] == 0) break
      par_id <- par[[1]]
      par_row <- pd[pd$id == par_id, , drop = FALSE]
      txt <- par_row$text[[1]]
      if (grepl("(", txt, fixed = TRUE)) {
        fn <- sub("\\(.*", "", trimws(txt))
        fn <- sub(".*::", "", fn)
        fn <- gsub("`", "", fn)
        if (!(fn %in% c("ns", "NS"))) {
          ranges[[length(ranges) + 1]] <- c(par_row$line1[[1]], par_row$line2[[1]])
          break
        }
      }
      cur <- par_id
    }
  }
  if (length(ranges) == 0) return(c(ln, ln))
  ranges[[which.min(vapply(ranges, function(r) r[[2]] - r[[1]], numeric(1)))]]
}

#' Merge UI interaction coverage into the same coverage object as R lines
#'
#' UI-construction code (`ui <- fluidPage(sliderInput("bins", ...), ...)`)
#' executes unconditionally at app startup, so ordinary R line coverage
#' always shows it as "covered" regardless of whether a test ever
#' interacted with any particular input/output. Rather than report UI
#' interaction as a second, separate metric, this adds one synthetic
#' coverage entry per UI element -- keyed to that element's own source
#' line, with a value driven by the interaction log instead of execution --
#' directly into `cov`, so `covr::percent_coverage()`, `covr::report()`,
#' and `covr::to_cobertura()` all reflect server logic and UI interaction
#' as one blended signal.
#'
#' This relies on how `covr` aggregates per-line coverage
#' (`covr:::tally_coverage(..., by = "line")`, what `percent_coverage()`
#' uses by default): entries expand to one row per source line and combine
#' with `FUN = min` within each `filename + functions + line` group. A
#' same-line, same-`functions` entry with `value = 0` therefore pulls an
#' untested element's line down to uncovered via `min()`.
#'
#' Before that `min()` stage runs, `covr:::as.data.frame.coverage()` calls
#' `covr:::merge_values()`, which *sums* the `value` of any two entries
#' whose full 8-number srcref tuple, `filename`, and `functions` are all
#' identical. For a UI element that's a complete, standalone one-line
#' statement (the idiomatic Shiny style), a whole-line synthetic srcref can
#' be byte-for-byte identical to the real covr entry for that statement, so
#' `merge_values()` sums the untested `value = 0` into the real nonzero
#' value (`1 + 0 = 1`) before `min()` ever sees two rows -- silently
#' erasing the untested signal. To prevent this, the synthetic entry's
#' srcref sets its two "parsed" components (used by `merge_values()`'s
#' grouping, but with no effect on line placement) to a distinct negative
#' sentinel per element -- a value no real parser-produced srcref can take
#' -- so it can never be silently summed into another entry.
#'
#' Known limitation: line-based reporting can only carry one verdict per
#' physical line. If two different manifest elements resolve to the same
#' line and disagree on tested status, the by-line `min()` correctly (if
#' coarsely) marks the line uncovered, but a reader can't tell which
#' element was untested from the blended report alone.
#' `merge_ui_coverage()` `message()`s when it detects this, pointing at
#' `ui_report()`'s per-element table as the disambiguating source of
#' truth -- except when any colliding element was placed via
#' `locate_by_srcref()`'s fallback rather than an exact text match, since
#' the fallback can attribute an id to a line it has nothing to do with
#' (see that hook's comment in bootstrap.R); in that case the message is
#' suppressed rather than asserted with false confidence.
#'
#' @param cov Coverage list from build_coverage().
#' @param app_dir Path to the app directory.
#' @return `cov` with additional synthetic entries, one per UI manifest
#'   element that could be located in the source.
#' @keywords internal
merge_ui_coverage <- function(cov, app_dir) {
  manifest_path <- manifest_read_path(app_dir)
  interactions_path <- file.path(shinycov_output_dir(app_dir), "interactions.json")

  if (!file.exists(manifest_path)) {
    message("shiny.cov: no UI manifest found for ", app_dir,
            " -- skipping UI-aware coverage adjustment.")
    return(cov)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(cov)

  manifest <- tryCatch(
    read_manifest_from_path(manifest_path),
    error = function(e) {
      message("shiny.cov: manifest at ", manifest_path,
              " exists but could not be parsed as JSON (", conditionMessage(e),
              ") -- skipping UI-aware coverage adjustment.")
      NULL
    })
  if (is.null(manifest)) return(cov)

  id_srcrefs <- read_id_srcrefs(app_dir)

  interactions <- if (file.exists(interactions_path)) {
    tryCatch(jsonlite::fromJSON(interactions_path, simplifyVector = FALSE),
             error = function(e) {
               message("shiny.cov: interactions.json at ", interactions_path,
                       " exists but could not be parsed as JSON (",
                       conditionMessage(e),
                       ") -- treating all UI elements as untested.")
               list()
             })
  } else {
    message("shiny.cov: no interaction log found at ", interactions_path,
            " -- treating all UI elements as untested. If you're using ",
            "Cypress, check that config.env.shinyCovAppDir/shinyCovOutputDir ",
            "resolves to the same .shiny.cov/ directory shiny.cov::setup() used.")
    list()
  }

  # Count interactions per element id (same action vocabulary used by
  # build_ui_coverage() in R/report.R -- kept in sync via R/utils.R so the
  # detailed per-element breakdown and this merged coverage number always
  # agree on what counts as "interacted").
  allowed_actions <- c(shinycov_input_actions(), shinycov_output_actions())
  hit_count <- list()
  for (x in interactions) {
    # x$selector/x$action should each be a length-1 character string, but a
    # malformed interactions.json entry (e.g. a logged NULL serializes via
    # jsonlite as an empty list `{}`, not JSON null) must degrade to "skip
    # this entry" rather than crash collect() for the whole app. `%||%`
    # alone only catches an actual NULL; a non-NULL empty list sails
    # through, and grepl()/`%in%` on it return logical(0), which makes the
    # enclosing `if (...)` throw "missing value where TRUE/FALSE needed".
    # Coerce both to "" whenever they aren't a proper scalar string.
    sel <- x$selector %||% ""
    if (!is.character(sel) || length(sel) != 1 || is.na(sel)) sel <- ""
    act <- x$action %||% ""
    if (!is.character(act) || length(act) != 1 || is.na(act)) act <- ""
    if (grepl("^#", sel) && act %in% allowed_actions) {
      id <- sub("^#", "", sel)
      hit_count[[id]] <- (hit_count[[id]] %||% 0L) + 1L
    }
  }

  ui_elements <- c(manifest$inputs, manifest$outputs)
  if (length(ui_elements) == 0) return(cov)

  files <- unique(vapply(cov, function(x) {
    sr <- x$srcref
    if (inherits(sr, "srcref")) {
      sf <- attr(sr, "srcfile", exact = TRUE)
      if (!is.null(sf)) return(sf$filename)
    }
    ""
  }, character(1)))
  files <- files[nzchar(files)]
  files <- files[file.exists(files)]

  file_lines <- list()
  file_entries <- list()
  file_srcfiles <- list()
  for (f in files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) next
    entries <- Filter(function(x) {
      sr <- x$srcref
      inherits(sr, "srcref") &&
        identical(attr(sr, "srcfile", exact = TRUE)$filename, f)
    }, cov)
    if (length(entries) == 0) next
    file_lines[[f]] <- lines
    file_entries[[f]] <- entries
    file_srcfiles[[f]] <- attr(entries[[1]]$srcref, "srcfile", exact = TRUE)
  }

  # Literal text search finds the exact line an id's string literal
  # appears on; it just can't find *computed* ids (`paste0("bin_", i)`
  # inside a loop has no literal string to match). The htmltools::tag()
  # hook (id_srcrefs, from bootstrap.R) can find those, but is generally
  # coarser for the common static case (it can only be as fine-grained as covr's own
  # per-statement instrumentation, which doesn't subdivide a flat
  # `ui <- fluidPage(sliderInput("bins", ...), ...)` expression at all).
  # So text search goes first; the hook is only a fallback for what it
  # can't find at all, not a replacement.
  locate_by_text <- function(id) {
    # `id` comes from the manifest (whatever string literal a developer
    # passed to sliderInput()/actionButton()/etc.) and can contain regex
    # metacharacters. regex_escape() neutralizes them so `id` always
    # matches as a literal string -- otherwise e.g. "a.b" would also match
    # "aXb", and "a(b" would throw an unguarded "invalid regular
    # expression" error.
    #
    # Some frameworks report a DOM id that never appears verbatim in the R
    # source: shiny.fluent appends `-input` and Shiny namespaces the id
    # (e.g. `app-inputs-kpi_years-input`), while the source only contains
    # the local id passed to ns() (`ns("kpi_years")`). Try progressively
    # less-qualified forms so these still map back to their source line.
    stripped <- sub("-input$", "", id)
    candidates <- unique(c(id, stripped, sub("^.*-", "", stripped)))
    for (cid in candidates) {
      if (!nzchar(cid)) next
      pattern <- sprintf('["\']%s["\']', regex_escape(cid))
      for (f in names(file_lines)) {
        matched <- which(vapply(file_lines[[f]], function(l) grepl(pattern, l), logical(1)))
        if (length(matched) > 0) return(list(file = f, line = matched[[1]]))
      }
    }
    NULL
  }

  locate_by_srcref <- function(id) {
    sr <- id_srcrefs[[id]]
    if (is.null(sr) || !inherits(sr, "srcref")) return(NULL)
    f <- attr(sr, "srcfile", exact = TRUE)$filename %||% ""
    if (!nzchar(f) || is.null(file_lines[[f]])) return(NULL)
    ln <- sr[[1]]
    if (ln < 1 || ln > length(file_lines[[f]])) return(NULL)
    list(file = f, line = ln)
  }

  new_entries <- list()
  # Tracks, per physical "file:line", every manifest element id resolved to
  # it and whether that element was interacted with -- used below to warn
  # about the granularity limitation where two different elements sharing
  # one physical source line necessarily share one line-level coverage verdict.
  line_status <- list()
  # Monotonically-decreasing sentinel counter -- see its use below. Every
  # element gets its own value so that two *different* elements which
  # happen to resolve to the same line don't ALSO end up with
  # byte-for-byte identical synthetic srcrefs to each other (both spanning
  # the same whole line, same byte range, same functions group): if they
  # did, `covr:::merge_values()` would sum their values together (e.g.
  # tested=1 + untested=0 = 1) *before* the by-line min() reduction ever
  # runs, masking the untested one inside a nonzero sum instead of letting
  # min() correctly flag the line as uncovered.
  synthetic_seq <- 0L
  # ids from `ui_elements` that neither locate_by_text() nor
  # locate_by_srcref() could place on any source line. Excluded from `cov`
  # below rather than counted as untested, which would otherwise bias the
  # blended percentage toward looking more complete than the manifest
  # reflects -- tracked here so the message() below can report it.
  unlocated_ids <- character(0)

  for (el in ui_elements) {
    id <- el$id
    if (is.null(id) || !nzchar(id)) next

    text_loc <- locate_by_text(id)
    loc <- text_loc %||% locate_by_srcref(id)
    if (is.null(loc)) {
      unlocated_ids <- c(unlocated_ids, id)
      next
    }
    # Whether this id was placed via the exact literal-text match (high
    # confidence -- the id's own string literal was found right on this
    # line) or via locate_by_srcref()'s fallback (bootstrap.R's
    # htmltools::tag() hook). The fallback records whichever *instrumented
    # statement last executed*, not necessarily this id's own call site --
    # correct for the common case (a computed id that's its own
    # block-member statement in a loop) but capable of attributing a bare,
    # nested call argument (e.g. `textOutput(ns("total"))` as one argument
    # among several) to a completely unrelated statement that merely
    # happened to run most recently (see bootstrap.R's "id -> srcref
    # attribution" comment block). Tracked per element below so the
    # same-line message doesn't assert a "shared line" with confidence it
    # doesn't have.
    via_fallback <- is.null(text_loc)

    f <- loc$file
    ln <- loc$line
    lines <- file_lines[[f]]
    entries <- file_entries[[f]]
    srcfile <- file_srcfiles[[f]]

    overlapping <- Filter(function(x) {
      sr <- x$srcref
      sr[[1]] <= ln && sr[[3]] >= ln
    }, entries)

    groups <- if (length(overlapping) > 0) {
      unique(lapply(overlapping, function(x) x$functions %||% character(0)))
    } else {
      list(character(0))
    }

    value <- hit_count[[id]] %||% 0L
    # Expand the synthetic entry to the whole enclosing widget expression
    # (multi-line Fluent/React widgets, etc.), not just the id literal's
    # line, so an untested widget shows up uncovered across all its lines.
    rng <- enclosing_expr_range(lines, ln)
    start_line <- rng[[1]]
    end_line <- rng[[2]]
    line_text <- lines[[end_line]]
    # The last two components of an srcref's 8-number vector are the
    # "parsed" first/last line, which for any *real* parser-produced srcref
    # is always a positive line number, equal to first_line/last_line for
    # the single- and multi-line statements this function deals with (see
    # the docstring above for the full 8-tuple story). A distinct
    # negative value here -- a value no real R source position can ever
    # take -- guarantees this synthetic entry's 8-tuple can never exactly
    # equal a real covr entry's (fixing the single-line collision that
    # silently dropped the untested signal), AND, because it's unique per
    # *element* (not a shared constant), guarantees two different elements
    # that resolve to the same line don't collide with each other either
    # (see synthetic_seq's own comment above). Either way,
    # `covr:::merge_values()`'s pre-aggregation `aggregate(value ~ ., x, sum)`
    # (grouped on the full 8-tuple + filename + functions) can never
    # silently sum entries together before the by-line `min()` reduction
    # runs. first_line/last_line (components 1 and 3, left untouched) are
    # what actually control which physical line this entry lands on for
    # that by-line reduction (via covr's `expand_lines()`), so this
    # sentinel has no effect on placement -- only on preventing accidental
    # identity with another entry.
    synthetic_seq <- synthetic_seq - 1L
    new_sr <- srcref(
      srcfile,
      c(start_line, 1L, end_line, max(1L, nchar(line_text)), 1L, max(1L, nchar(line_text)),
        synthetic_seq, synthetic_seq)
    )

    for (i in seq_along(groups)) {
      key <- paste("ui", id, f, ln, i, sep = ":")
      new_entries[[key]] <- list(srcref = new_sr, value = value, functions = groups[[i]])
    }

    loc_key <- paste(f, ln, sep = ":")
    line_status[[loc_key]] <- c(
      line_status[[loc_key]],
      list(list(id = id, tested = value > 0, via_fallback = via_fallback))
    )
  }

  if (length(unlocated_ids) > 0) {
    shown <- utils::head(unlocated_ids, 10L)
    message(
      "shiny.cov: could not locate a source line for ", length(unlocated_ids),
      " UI element", if (length(unlocated_ids) == 1L) "" else "s",
      " from the manifest (", paste(shown, collapse = ", "),
      if (length(unlocated_ids) > length(shown)) ", ..." else "", ") -- ",
      "these are excluded from the blended coverage number entirely, which ",
      "biases it toward looking more complete than the manifest actually ",
      "reflects. This typically means the id is computed (e.g. paste0() ",
      "inside a loop) with no id_srcrefs attribution recorded for it, or ",
      "its string literal doesn't appear verbatim anywhere in the app's R ",
      "source."
    )
  }

  # Line-based coverage can only show one verdict per physical line. If two
  # different manifest elements share a line and disagree on tested status,
  # by-line min() correctly marks the line uncovered but a reader can't
  # tell which element was untested from the blended report alone --
  # surface it here so it doesn't look like ordinary min-reduction noise.
  for (loc_key in names(line_status)) {
    elements <- line_status[[loc_key]]
    if (length(elements) < 2) next
    ids <- vapply(elements, function(e) e$id, character(1))
    tested <- vapply(elements, function(e) e$tested, logical(1))
    via_fallback <- vapply(elements, function(e) e$via_fallback, logical(1))
    if (length(unique(ids)) > 1 && length(unique(tested)) > 1) {
      # Only claim "these elements share a line" when every element here
      # was placed via the high-confidence text match -- a fallback
      # participant means the line attribution itself might be wrong (see
      # via_fallback's comment above), so stay silent rather than send a
      # developer to inspect an unrelated line.
      if (any(via_fallback)) next
      message(
        "shiny.cov: elements ", paste(unique(ids), collapse = ", "),
        " all resolve to ", loc_key, " but differ in tested status -- ",
        "line-based coverage can only show one verdict for that line ",
        "(the untested element wins). See ui_report()'s per-element table ",
        "for the disambiguated, per-id breakdown."
      )
    }
  }

  for (k in names(new_entries)) {
    cov[[k]] <- new_entries[[k]]
  }
  cov
}

#' Read the id -> srcref attribution RDS file, if present
#' @param app_dir Path to the Shiny app directory.
#' @return Named list (id -> srcref), possibly empty.
#' @keywords internal
read_id_srcrefs <- function(app_dir) {
  path <- shinycov_id_srcrefs_file(app_dir)
  if (!file.exists(path)) return(list())
  tryCatch(readRDS(path), error = function(e) list())
}
