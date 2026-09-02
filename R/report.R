#' Generate a coverage report
#'
#' Produces a self-contained HTML report showing, per source file, every
#' line with its hit count in the left gutter -- including a per-test-source
#' breakdown (`total (cypress=N shinytest2=M)`) when the run was tagged with
#' `SHINYCOV_SOURCE` -- plus the per-element UI breakdown. `x` already has UI
#' interaction coverage merged directly into it (see [merge_ui_coverage()]),
#' so [covr::percent_coverage(x)] and this report are one blended number.
#'
#' @param x A coverage object returned by [collect()].
#' @param file Output `.html` path (default `"coverage-report/index.html"`).
#' @param app_dir Path to the app directory for the UI manifest and
#'   interaction log. Defaults to `"."`.
#' @param ... Ignored (kept for compatibility).
#'
#' @return The HTML report path (invisibly).
#' @export
report <- function(x, file = "coverage-report/index.html", app_dir = ".", ...) {
  if (!inherits(x, "coverage")) {
    stop("`x` must be a coverage object from shiny.cov::collect().")
  }

  pct <- covr::percent_coverage(x)
  ui  <- tryCatch(build_ui_coverage(load_manifest(app_dir),
                                    load_interactions(NULL, app_dir)),
                  error = function(e) NULL)

  message("")
  message("-- shiny.cov coverage: ", sprintf("%.1f%%", pct),
          " (R lines + UI elements, combined) --")
  if (!is.null(ui)) print_ui_report(ui)

  file <- render_report_html(x, ui, file)
  message(sprintf("  HTML: %s", normalizePath(file, winslash = "/")))
  message("")

  invisible(file)
}

render_report_html <- function(cov, ui, file) {
  pct <- covr::percent_coverage(cov)
  df <- source_coverage(cov)
  source_cols <- setdiff(names(df), c("filename", "line", "total"))

  html <- c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'><style>",
    "body{font-family:monospace;margin:1em;background:#fff}",
    "h1,h2{font-family:sans-serif}",
    ".ui{font-family:sans-serif;font-size:13px}",
    ".file{margin:1.5em 0;border:1px solid #ddd}",
    ".file h3{margin:0;padding:.4em .6em;background:#f0f0f0;font-size:14px;font-family:sans-serif}",
    ".line{display:flex;white-space:pre;font-size:12px;line-height:1.25}",
    ".gutter{min-width:18em;padding-right:1em;color:#333;text-align:right;border-right:1px solid #eee}",
    ".lineno{min-width:3.5em;padding:0 .8em;color:#aaa;text-align:right}",
    ".code{white-space:pre;padding-left:.5em}",
    ".uncovered{background:#fcece9}",
    "</style></head><body>",
    sprintf("<h1>shiny.cov coverage &mdash; %.1f%%</h1>", pct)
  )

  if (!is.null(ui)) {
    html <- c(html, "<h2>UI elements</h2><ul class='ui'>")
    for (inp in ui$inputs) {
      st <- if (isTRUE(inp$interacted)) sprintf("[OK] interacted (%d x)", inp$count) else "[--] never interacted"
      mod <- if (nzchar(inp$module %||% "")) sprintf(" [%s]", inp$module) else ""
      html <- c(html, sprintf("<li>%s (%s)%s &mdash; %s</li>", inp$id, inp$type %||% "unknown", mod, st))
    }
    for (out in ui$outputs) {
      st <- if (isTRUE(out$verified)) sprintf("[OK] verified (%d x)", out$count) else "[--] not verified"
      mod <- if (nzchar(out$module %||% "")) sprintf(" [%s]", out$module) else ""
      html <- c(html, sprintf("<li>%s (%s)%s &mdash; %s</li>", out$id, out$type %||% "unknown", mod, st))
    }
    html <- c(html, "</ul>")
  }

  for (f in unique(df$filename)) {
    if (!file.exists(f)) next
    sub <- df[df$filename == f, , drop = FALSE]
    src_lines <- readLines(f, warn = FALSE)
    f_covered <- sum(sub$total > 0)
    f_pct <- if (nrow(sub) > 0) round(100 * f_covered / nrow(sub), 1) else 100
    html <- c(html, sprintf("<div class='file'><h3>%s &mdash; %s%%</h3>", f, f_pct))
    for (ln in seq_along(src_lines)) {
      vals <- numeric(length(source_cols))
      names(vals) <- source_cols
      for (s in source_cols) {
        v <- sub[[s]][sub$line == ln]
        vals[[s]] <- if (length(v) == 0) 0 else v
      }
      total <- if (length(source_cols) == 0) {
        v <- sub$total[sub$line == ln]
        if (length(v) == 0) 0 else v
      } else {
        sum(vals)
      }
      if (length(source_cols) <= 1) {
        gutter <- as.character(total)
      } else {
        gutter <- paste0(total, " (", paste(paste0(source_cols, "=", vals), collapse = " "), ")")
      }
      cls <- if (total == 0) "uncovered" else ""
      esc <- gsub("&", "&amp;", src_lines[[ln]])
      esc <- gsub("<", "&lt;", esc)
      esc <- gsub(">", "&gt;", esc)
      html <- c(html, sprintf(
        "<div class='line %s'><span class='gutter'>%s</span><span class='lineno'>%d</span><span class='code'>%s</span></div>",
        cls, gutter, ln, esc
      ))
    }
    html <- c(html, "</div>")
  }
  html <- c(html, "</body></html>")
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, file)
  invisible(file)
}

#' UI interaction coverage report
#'
#' Shows which Shiny inputs were interacted with, which outputs were
#' verified, and which tabs were visited during testing. This is data
#' `covr::report()` cannot provide because it only tracks R line execution.
#'
#' @param x A coverage object from [collect()].
#' @param interaction_log Path to interaction log JSON.
#' @param app_dir Path to the app directory.
#'
#' @return Invisibly returns a list with UI coverage data.
#' @export
ui_report <- function(x, interaction_log = NULL, app_dir = ".") {
  if (!inherits(x, "coverage")) {
    stop("`x` must be a coverage object from shiny.cov::collect().")
  }

  manifest     <- load_manifest(app_dir)
  interactions <- load_interactions(interaction_log, app_dir)
  ui           <- build_ui_coverage(manifest, interactions)

  print_ui_report(ui)
  invisible(ui)
}

# ---- Helpers ----

load_manifest <- function(app_dir) {
  path <- manifest_read_path(app_dir)
  if (!file.exists(path)) {
    message("No UI manifest found")
    return(default_manifest())
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(default_manifest())
  }
  m <- tryCatch(read_manifest_from_path(path),
                error = function(e) default_manifest())
  for (f in c("inputs", "outputs", "tabs", "conditional", "modules")) {
    if (is.null(m[[f]])) m[[f]] <- if (f %in% c("inputs", "outputs")) list() else character()
  }
  apply_module_boundaries(m, app_dir)
}

default_manifest <- function() {
  list(inputs = list(), outputs = list(), tabs = character(),
       conditional = character(), modules = character())
}

#' Populate manifest$modules and each element's module field from ground truth
#'
#' The browser-side discovery script can see the "-"-joined id
#' strings Shiny puts in the DOM, but can't tell a real module namespace
#' boundary hyphen from one an app author just happened to put in a plain
#' top-level id -- only the R-side runtime, where `shiny::moduleServer()`
#' actually runs, knows that for certain (see the hook in `bootstrap.R`).
#' Reads the RDS file that hook writes and, if present, uses it as ground
#' truth instead of leaving `modules`/`module` empty or guessed.
#'
#' That ground truth is a flat list of real boundary prefixes, though, not
#' a per-id map -- see the corroboration check inside for the residual
#' (documented, accepted) ambiguity that remains for a top-level id that
#' merely *coincides* with a real module's prefix.
#'
#' @param m Manifest list (from [load_manifest()]).
#' @param app_dir Path to the app directory.
#' @return `m`, with `modules` and each input/output's `module` populated.
#' @keywords internal
apply_module_boundaries <- function(m, app_dir) {
  path <- shinycov_modules_file(app_dir)
  if (!file.exists(path)) return(m)
  boundaries <- tryCatch(readRDS(path), error = function(e) character())
  if (length(boundaries) == 0) return(m)

  # Longest match wins, so a nested module ("outer-inner") is attributed
  # to itself rather than its parent ("outer"). This is correct whenever
  # the match reflects genuine nesting (the boundary really was reached by
  # composing session$ns() calls -- see the moduleServer() hook this
  # reads from, in bootstrap.R).
  boundaries <- boundaries[order(nchar(boundaries), decreasing = TRUE)]

  # `boundaries` only records which id prefixes are real module namespaces
  # -- not which specific ids live under each one. A plain top-level input
  # named e.g. "sidebar-toggle" is indistinguishable, from this data alone,
  # from a genuine child of a module called "sidebar": Shiny's DOM carries
  # no marker for "composed via ns()" vs. "hyphen typed literally". So a
  # prefix match is only trusted if at least one *other* id in this
  # manifest also falls under the same boundary (an exact id == boundary
  # match is trusted unconditionally, since that's not a coincidence). A
  # boundary with no corroborating element is left unattributed (module =
  # "") rather than guessed; this doesn't resolve every ambiguous case (a
  # documented, accepted limitation) but affects only the cosmetic module
  # grouping in ui_report(), never the blended coverage percentage.
  # Excludes by *position*, not by id value, so two distinct elements that
  # happen to share one id can still corroborate each other.
  all_ids <- c(
    vapply(m$inputs, function(x) x$id %||% "", character(1)),
    vapply(m$outputs, function(x) x$id %||% "", character(1))
  )
  has_corroboration <- function(b, self_idx) {
    other <- all_ids[-self_idx]
    any(other == b | startsWith(other, paste0(b, "-")))
  }

  owning_module <- function(id, idx) {
    for (b in boundaries) {
      if (identical(id, b)) return(b)
      if (startsWith(id, paste0(b, "-")) && has_corroboration(b, idx)) return(b)
    }
    ""
  }

  n_inputs <- length(m$inputs)
  m$modules <- boundaries
  m$inputs  <- lapply(seq_along(m$inputs), function(i) {
    x <- m$inputs[[i]]
    x$module <- owning_module(x$id %||% "", i)
    x
  })
  m$outputs <- lapply(seq_along(m$outputs), function(i) {
    x <- m$outputs[[i]]
    x$module <- owning_module(x$id %||% "", n_inputs + i)
    x
  })
  m
}

load_interactions <- function(log, app_dir) {
  path <- if (is.null(log)) file.path(shinycov_output_dir(app_dir), "interactions.json") else log
  if (!file.exists(path)) { message("No interaction log found"); return(list()) }
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else list()
}

# A logged NULL selector/action serializes via jsonlite as an empty list
# `{}`, not JSON null -- `%||%` alone only catches an actual NULL, so a
# malformed entry sails through as a non-character, non-scalar value.
# grepl()/`%in%`/etc. on that return logical(0) instead of a scalar,
# which crashes any enclosing `&&` chain ("missing value where TRUE/FALSE
# needed" or "'length = 0' in coercion to 'logical(1)'"). Coerce to "" so
# a malformed entry just fails to match instead of crashing report()/
# ui_report() for the whole app.
ui_safe_str <- function(x) {
  x <- x %||% ""
  if (!is.character(x) || length(x) != 1 || is.na(x)) "" else x
}

build_ui_coverage <- function(manifest, interactions) {
  input_actions  <- shinycov_input_actions()
  output_actions <- shinycov_output_actions()

  input_coverage <- lapply(manifest$inputs, function(inp) {
    hits <- vapply(interactions, function(x) {
      identical(sub("^#", "", ui_safe_str(x$selector)), inp$id) &&
        ui_safe_str(x$action) %in% input_actions
    }, logical(1))
    list(id = inp$id, type = inp$type %||% "unknown", module = inp$module %||% "",
         label = inp$label %||% "", interacted = any(hits), count = sum(hits))
  })

  output_coverage <- lapply(manifest$outputs, function(out) {
    hits <- vapply(interactions, function(x) {
      identical(sub("^#", "", ui_safe_str(x$selector)), out$id) &&
        ui_safe_str(x$action) %in% output_actions
    }, logical(1))
    list(id = out$id, type = out$type %||% "unknown", module = out$module %||% "",
         verified = any(hits), count = sum(hits))
  })

  tab_coverage <- lapply(manifest$tabs, function(tab) {
    list(title = tab, visited = any(vapply(interactions, function(x) {
      identical(ui_safe_str(x$action), "click") &&
        grepl(tab, ui_safe_str(x$selector), fixed = TRUE)
    }, logical(1))))
  })

  inputs_total  <- length(input_coverage)
  inputs_hit    <- sum(vapply(input_coverage, `[[`, logical(1), "interacted"))
  outputs_total <- length(output_coverage)
  outputs_hit   <- sum(vapply(output_coverage, `[[`, logical(1), "verified"))
  tabs_total    <- length(tab_coverage)
  tabs_hit      <- sum(vapply(tab_coverage,   `[[`, logical(1), "visited"))

  list(
    inputs = input_coverage, outputs = output_coverage, tabs = tab_coverage,
    summary = list(
      inputs_total = inputs_total, inputs_hit = inputs_hit,
      inputs_pct   = if (inputs_total > 0)  round(100 * inputs_hit   / inputs_total, 1) else NA,
      outputs_total = outputs_total, outputs_hit = outputs_hit,
      outputs_pct  = if (outputs_total > 0) round(100 * outputs_hit  / outputs_total, 1) else NA,
      tabs_total   = tabs_total,     tabs_hit = tabs_hit,
      tabs_pct     = if (tabs_total > 0)    round(100 * tabs_hit     / tabs_total,   1) else NA
    )
  )
}

print_ui_report <- function(r) {
  s <- r$summary
  cat("\n")
  mod_suffix <- function(module) if (nzchar(module %||% "")) sprintf(" [%s]", module) else ""
  if (s$inputs_total > 0) {
    for (inp in r$inputs) {
      status <- if (inp$interacted) sprintf("[OK] interacted (%d x)", inp$count) else "[--] never interacted"
      cat(sprintf("  %-40s %s\n", sprintf("%s (%s)%s", inp$id, inp$type, mod_suffix(inp$module)), status))
    }
  }
  if (s$outputs_total > 0) {
    for (out in r$outputs) {
      status <- if (out$verified) sprintf("[OK] verified (%d x)", out$count) else "[--] not verified"
      cat(sprintf("  %-40s %s\n", sprintf("%s (%s)%s", out$id, out$type, mod_suffix(out$module)), status))
    }
  }
  if (s$tabs_total > 0) {
    for (tab in r$tabs) {
      status <- if (tab$visited) "[OK] visited" else "[--] never visited"
      cat(sprintf("  %-40s %s\n", tab$title, status))
    }
  }
  cat("\n")
}

#' Per-source coverage summary
#'
#' Returns a data frame with one row per test source that produced coverage
#' (e.g. `shinytest2`, `cypress`, `playwright`), showing the number of tracked
#' expressions and the total hit count contributed by each source. For a run
#' with a single (untagged) source, the only row is `total`.
#'
#' @param cov A coverage object returned by [collect()].
#'
#' @return A `data.frame` with columns `source`, `expressions`, and `hits`.
#' @export
source_counts <- function(cov) {
  sources <- attr(cov, "shinycov_sources", exact = TRUE)
  if (is.null(sources)) {
    return(data.frame(
      source = "total",
      expressions = length(cov),
      hits = sum(vapply(cov, function(e) e$value %||% 0L, numeric(1))),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(names(sources), function(s) {
    ctrs <- sources[[s]]
    data.frame(
      source = s,
      expressions = length(ctrs),
      hits = sum(vapply(ctrs, function(e) {
        if (is.list(e)) e$value %||% 0L else if (is.numeric(e)) as.numeric(e) else 0
      }, numeric(1))),
      stringsAsFactors = FALSE
    )
  }))
}

#' Per-source per-line coverage
#'
#' Returns a data frame with one row per measured `(file, line)` and one
#' column per test source giving that source's hit count, plus a `total`
#' column. Requires the coverage object to carry per-source data (see
#' [collect()]).
#'
#' @param cov A coverage object returned by [collect()].
#' @return A `data.frame`, or `NULL` when no per-source data is present.
#' @export
source_coverage <- function(cov) {
  sources <- attr(cov, "shinycov_sources", exact = TRUE)
  if (is.null(sources)) {
    df <- covr::tally_coverage(cov, by = "line")
    agg <- stats::aggregate(df$value, by = list(filename = df$filename, line = df$line), FUN = min)
    names(agg)[3] <- "total"
    return(agg[order(agg$filename, agg$line), ])
  }

  out <- NULL
  for (src in names(sources)) {
    env <- reconstruct_counters(sources[[src]])
    cov_s <- build_coverage(env)
    class(cov_s) <- c("coverage", "list")
    df <- covr::tally_coverage(cov_s, by = "line")
    agg <- stats::aggregate(df$value, by = list(filename = df$filename, line = df$line), FUN = min)
    names(agg)[3] <- src
    out <- if (is.null(out)) agg else merge(out, agg, by = c("filename", "line"), all = TRUE)
  }
  src_cols <- names(sources)
  for (s in src_cols) out[[s]][is.na(out[[s]])] <- 0L
  out$total <- rowSums(out[src_cols])
  out[order(out$filename, out$line), , drop = FALSE]
}

