#' Generate a coverage report
#'
#' Produces a covr-style HTML report with a per-file overview (`Files` tab)
#' and a per-file source view (`Source` tab). In the source view the hit
#' count for every line is annotated with its per-test-source breakdown
#' (`2 (cypress=1 shinytest2=1)`) whenever the run was tagged with
#' `SHINYCOV_SOURCE`. `x` already has UI interaction coverage merged
#' directly into it (see [merge_ui_coverage()]), so
#' [covr::percent_coverage(x)] and this report are one blended number.
#'
#' @param x A coverage object returned by [collect()].
#' @param file Output `.html` path (default `"coverage-report/index.html"`;
#'   dependencies are written to a `lib/` directory alongside it).
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

  file <- render_report_html_covr(x, ui, file)
  message(sprintf("  HTML: %s", normalizePath(file, winslash = "/")))
  message("")

  invisible(file)
}

render_report_html_covr <- function(cov, ui, file) {
  data <- sc_report_to_data(cov)
  src <- source_coverage(cov)
  source_cols <- setdiff(names(src), c("filename", "line", "total"))

  color_coverage_callback <- DT::JS("function(td, cellData, rowData, row, col) {
  var percent = cellData.replace('%', '');
  if (percent > 90) {
    var grad = 'linear-gradient(90deg, #edfde7 ' + cellData + ', white ' + cellData + ')';
  } else if (percent > 75) {
    var grad = 'linear-gradient(90deg, #f9ffe5 ' + cellData + ', white ' + cellData + ')';
  } else {
    var grad = 'linear-gradient(90deg, #fcece9 ' + cellData + ', white ' + cellData + ')';
  }
  $(td).css('background', grad);
}")
  file_choice_callback <- DT::JS("table.on('click.dt', 'a', function() {
  files = $('div#files div');
  files.not('div.hidden').addClass('hidden');
  id = $(this).text();
  files.filter('div[id=\\'' + id + '\\']').removeClass('hidden');
  $('ul.nav a[data-value=Source]').text(id).tab('show');
});")
  pkg <- attr(cov, "package")$package %||% "shiny.cov"
  percentage <- sprintf("%02.2f%%", data$overall)

  table <- DT::datatable(data$file_stats, escape = FALSE, fillContainer = FALSE,
    options = list(searching = FALSE, dom = "t", paging = FALSE,
      columnDefs = list(list(targets = 6, createdCell = color_coverage_callback))),
    rownames = FALSE, class = "row-border", callback = file_choice_callback)
  table$sizingPolicy$defaultWidth <- "100%"
  table$sizingPolicy$defaultHeight <- NULL

  source_panel <- if (length(source_cols) > 0) {
    sc_report_tab_panel(
      "Source",
      htmltools::tags$label(
        class = "source-col-toggle",
        htmltools::tags$input(
          type = "checkbox", id = "shinycov-toggle-sources", checked = NA
        ),
        " show test source columns"
      ),
      sc_report_add_highlight(render_source_table(data$full, src, source_cols)),
      htmltools::tags$script(
        "$('#shinycov-toggle-sources').on('change', function() {
           var show = this.checked ? '' : 'none';
           $('#files .source-col').css('display', show);
         });"
      )
    )
  } else {
    sc_report_tab_panel("Source", sc_report_add_highlight(
      render_source_table(data$full, src, source_cols)
    ))
  }

  ui_html <- sc_report_fluid_page(
    htmltools::includeCSS(system.file("www/report.css", package = "covr")),
    sc_report_column(8, offset = 2, size = "md",
      htmltools::HTML(paste0("<h2>", pkg, " coverage - ", percentage, "</h2>")),
      sc_report_tabset_panel(
        sc_report_tab_panel("Files", table),
        source_panel
      )
    )
  )
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  htmltools::save_html(ui_html, file)
  invisible(file)
}

render_source_table <- function(full, src, source_cols) {
  has_src <- length(source_cols) > 0 && !is.null(src)
  htmltools::div(id = "files", Map(function(lines, file) {
    sub <- if (has_src) src[src$filename == file, , drop = FALSE] else NULL

    thead <- NULL
    if (has_src) {
      head_cells <- c(
        list(htmltools::tags$th(class = "num", "#"),
             htmltools::tags$th(class = "coverage", "count")),
        lapply(source_cols, function(s) {
          htmltools::tags$th(class = "source-col", s)
        }),
        list(htmltools::tags$th(class = "col-sm-12", "source"))
      )
      thead <- htmltools::tags$thead(htmltools::tags$tr(head_cells))
    }

    tbody <- htmltools::tags$tbody(lapply(seq_len(NROW(lines)), function(row_num) {
      line_no <- lines[row_num, "line"]

      # Per-line total and per-source counts come from source_coverage(),
      # which runs covr's own per_line() algorithm per source and then sums
      # the sources, so the "count" column is always the per-source columns
      # added together (and never/missed/covered agree across columns).
      if (has_src && !is.null(sub)) {
        v <- sub[sub$line == line_no, , drop = FALSE]
        if (nrow(v) == 1) {
          coverage <- v$total
          per_src <- vapply(source_cols, function(sc) {
            x <- v[[sc]]
            if (is.na(x)) "" else as.character(x)
          }, character(1))
        } else {
          coverage <- ""
          per_src <- rep("", length(source_cols))
        }
      } else {
        coverage <- lines[row_num, "coverage"]
        per_src <- character(0)
      }

      cov_type <- NULL
      if (coverage == 0) {
        cov_value <- "!"
        cov_type <- "missed"
      } else if (coverage > 0) {
        cov_value <- htmltools::HTML(paste0(coverage, "<em>x</em>", collapse = ""))
        cov_type <- "covered"
      } else {
        cov_value <- ""
        cov_type <- "never"
      }

      src_cells <- if (has_src) {
        lapply(per_src, function(sv) {
          htmltools::tags$td(class = "source-col", sv)
        })
      } else {
        list()
      }

      htmltools::tags$tr(class = cov_type,
        htmltools::tags$td(class = "num", line_no),
        htmltools::tags$td(class = "coverage", cov_value),
        src_cells,
        htmltools::tags$td(class = "col-sm-12",
          htmltools::pre(class = "language-r", lines[row_num, "source"])))
    }))

    htmltools::div(id = file, class = "hidden",
      htmltools::tags$table(class = "table-condensed", thead, tbody))
  }, lines = full, file = names(full)),
  htmltools::tags$style(
    "#files thead th{position:sticky;top:1.9em;background:#f5f5f5;z-index:1;border-bottom:1px solid #ddd}",
    "#files th.num,#files th.coverage,#files th.source-col{text-align:right}",
    "#files td.source-col{text-align:right;min-width:4.5em;padding:0 0.6em}",
    ".source-col-toggle{position:sticky;top:0;z-index:2;background:#fff;display:block;padding:.2em 0}"
  ),
  htmltools::tags$script("$('div#files pre').each(function(i, block) {
    hljs.highlightBlock(block);
});"))
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
    return(full_line_df(sc_report_to_data(cov)$full, "total"))
  }

  out <- NULL
  for (src in names(sources)) {
    env <- reconstruct_counters(sources[[src]])
    cov_s <- build_coverage(env)
    class(cov_s) <- c("coverage", "list")
    df_s <- full_line_df(sc_report_to_data(cov_s)$full, src)
    out <- if (is.null(out)) df_s else merge(out, df_s, by = c("filename", "line"), all = TRUE)
  }

  src_cols <- names(sources)
  for (s in src_cols) {
    out[[s]][is.na(out[[s]])] <- ""
  }
  # A line is "never" (non-relevant) only when every source reports no value;
  # otherwise its total is the sum of the per-source hit counts, so the
  # "count" column always equals the per-source columns added together.
  never <- Reduce(`&`, lapply(out[src_cols], function(x) x == ""))
  nums <- lapply(out[src_cols], function(x) {
    n <- suppressWarnings(as.numeric(x))
    n[is.na(n)] <- 0
    n
  })
  out$total <- ifelse(never, "", as.character(Reduce(`+`, nums)))
  out[order(out$filename, out$line), , drop = FALSE]
}

full_line_df <- function(full, colname) {
  df <- do.call(rbind, lapply(names(full), function(f) {
    d <- full[[f]]
    data.frame(filename = f, line = d$line, value = d$coverage,
               stringsAsFactors = FALSE)
  }))
  names(df)[3] <- colname
  df
}

