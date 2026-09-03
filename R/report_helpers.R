# Report-data and HTML-chrome helpers, ported from {covr} (MIT licensed;
# https://github.com/r-lib/covr) so shiny.cov::report()/source_coverage()
# build their output from in-tree code rather than covr::: internals.
#
# The covr originals are to_report_data()/compute_file_stats()/
# sort_file_stats()/add_link()/addHighlight()/column()/tab_panel()/
# fluid_page()/tabset_panel() (covr R/report.R), per_line()/traced_files()/
# vcapply() (covr R/utils.R), and as.data.frame.coverage()/merge_values()
# (covr R/data_frame.R). Two deviations from the originals, both deliberate:
#   * `#line` directive support in traced_files() is dropped -- shiny.cov
#     only instruments plain .R files it parses itself (see the same note on
#     shinycov_get_parse_data() in R/trace.R).
#   * rex:: regexes are replaced with base R equivalents, avoiding a rex
#     dependency.

sc_report_vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, ..., FUN.VALUE = character(1))
}

# Internal equivalent of covr's as.data.frame.coverage(), kept as a plain
# function (not an S3 method) so it can't collide with covr's own registered
# method for the same class.
sc_report_coverage_df <- function(x, sort = TRUE) {
  column_names <- c(
    "filename", "functions", "first_line", "first_byte", "last_line",
    "last_byte", "first_column", "last_column", "first_parsed",
    "last_parsed", "value"
  )

  res <- stats::setNames(
    c(list(character(0)), rep(list(numeric(0)), times = length(column_names) - 1)),
    column_names
  )
  if (length(x)) {
    res$filename <- covr::display_name(x)
    res$functions <- sc_report_vcapply(x, function(xx) xx$functions[1])

    vals <- t(vapply(
      x,
      function(xx) c(xx$srcref, xx$value),
      numeric(9),
      USE.NAMES = FALSE
    ))
    for (i in seq_len(NCOL(vals))) {
      res[[i + 2]] <- vals[, i]
    }
  }

  df <- data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)

  if (sort) {
    df <- sc_report_merge_values(df)
    df <- df[order(df$filename, df$first_line, df$first_byte, df$last_line, df$last_byte), ]
  }

  rownames(df) <- NULL
  df
}

sc_report_merge_values <- function(x, sentinel = "___NA___") {
  if (NROW(x) == 0) {
    return(x)
  }
  # aggregate() can't group on missing values, so use a sentinel for NA
  # functions and restore it afterwards.
  x$functions[is.na(x$functions)] <- sentinel
  res <- stats::aggregate(value ~ ., x, sum)
  res$functions[res$functions == sentinel] <- NA_character_
  res
}

sc_report_traced_files <- function(x) {
  res <- list()
  filenames <- covr::display_name(x)
  for (i in seq_along(x)) {
    src_file <- attr(x[[i]]$srcref, "srcfile")
    filename <- filenames[[i]]

    if (filename == "") next
    if (!is.null(res[[filename]])) next

    src_file$file_lines <- getSrcLines(src_file, 1, Inf)
    res[[filename]] <- src_file
  }
  res
}

sc_report_per_line <- function(coverage) {
  df <- sc_report_coverage_df(coverage)

  # Generated code (e.g. onload) may have no source reference, so first_line
  # can be NA -- drop those rows.
  df <- df[!is.na(df$first_line), ]

  files <- sc_report_traced_files(coverage)

  # Lines that are blank or contain only a comment.
  blank_lines <- lapply(files, function(file) {
    which(grepl("^\\s*(#.*)?$", file$file_lines))
  })

  # Lines made only of punctuation/whitespace, or an `else` on its own.
  empty_lines <- lapply(files, function(file) {
    which(grepl("^(?:[[:punct:][:space:]]|else)*$", file$file_lines, perl = TRUE))
  })

  file_lengths <- lapply(files, function(file) length(file$file_lines))

  res <- lapply(file_lengths, function(x) rep(NA_real_, length.out = x))

  # df is sorted by file and first line ascending, so the maximum last_line
  # seen so far detects when the previous expression contains the current one.
  max_last <- 0
  prev_filename <- ""

  for (i in seq_len(NROW(df))) {
    filename <- df[i, "filename"]
    for (line in seq(df[i, "first_line"], df[i, "last_line"])) {
      if (!line %in% c(blank_lines[[filename]], empty_lines[[filename]])) {
        value <- df[i, "value"]
        if (is.na(res[[filename]][line]) || line < max_last ||
            (line == max_last && res[[filename]][line] > value)) {
          res[[filename]][line] <- value
        }

        if (df[i, "filename"] != prev_filename) {
          prev_filename <- df[i, "filename"]
          max_last <- 0
        }
        if (df[i, "last_line"] > max_last) {
          max_last <- df[i, "last_line"]
        }
      }
    }
  }

  Map(function(file, coverage) {
    list(file = file, coverage = coverage)
  }, files, res)
}

sc_report_to_data <- function(x) {
  coverages <- sc_report_per_line(x)

  res <- list()
  res$overall <- covr::percent_coverage(x)
  res$full <- lapply(coverages, function(coverage) {
    lines <- coverage$file$file_lines
    values <- coverage$coverage
    values[is.na(values)] <- ""
    data.frame(
      line = seq_along(lines),
      source = lines,
      coverage = values,
      stringsAsFactors = FALSE
    )
  })
  nms <- names(coverages)

  # Use a temporary name if a filename is empty.
  nms[nms == ""] <- "<text>"
  names(res$full) <- nms

  res$file_stats <- sc_report_compute_file_stats(res$full)
  res$file_stats$File <- sc_report_add_link(names(res$full))
  res$file_stats <- sc_report_sort_file_stats(res$file_stats)

  res
}

sc_report_compute_file_stats <- function(files) {
  do.call("rbind", lapply(files, function(file) {
    data.frame(
      Coverage = sprintf("%.2f%%", sum(file$coverage > 0) / sum(file$coverage != "") * 100),
      Lines = NROW(file),
      Relevant = sum(file$coverage != ""),
      Covered = sum(file$coverage > 0),
      Missed = sum(file$coverage == 0),
      `Hits / Line` = sprintf("%.0f", sum(as.numeric(file$coverage), na.rm = TRUE) / sum(file$coverage != "")),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
}

sc_report_sort_file_stats <- function(stats) {
  stats[order(as.numeric(sub("%", "", stats$Coverage)), -stats$Relevant),
        c("File", "Lines", "Relevant", "Covered", "Missed", "Hits / Line", "Coverage")]
}

sc_report_add_link <- function(files) {
  sc_report_vcapply(files, function(file) as.character(htmltools::a(href = "#", file)))
}

# ---- HTML chrome (adapted from covr, which adapted them from shiny) ----

sc_report_column <- function(width, ..., offset = 0, size = c("xs", "sm", "md", "lg")) {
  size <- match.arg(size)

  col_class <- paste0("col-", size, "-", width)
  if (offset > 0) {
    col_class <- paste0(col_class, " ", "col-", size, "-offset-", offset)
  }
  htmltools::div(class = col_class, ...)
}

sc_report_tab_panel <- function(title, ..., value = title) {
  htmltools::div(class = "tab-pane", title = title, `data-value` = value, ...)
}

sc_report_fluid_page <- function(...) {
  sc_report_bootstrap_page(htmltools::div(class = "container-fluid", ...))
}

sc_report_bootstrap_page <- function(...) {
  htmltools::attachDependencies(
    htmltools::tagList(list(...)),
    sc_report_html_dependency_bootstrap()
  )
}

sc_report_html_dependency_bootstrap <- function() {
  htmltools::htmlDependency(
    name = "bootstrap", version = "3.3.5",
    src = system.file(file = "www/shared/bootstrap", package = "covr"),
    meta = list(viewport = "width=device-width, initial-scale=1"),
    script = c("js/bootstrap.min.js", "shim/html5shiv.min.js", "shim/respond.min.js"),
    stylesheet = c("css/bootstrap.min.css", "css/bootstrap-theme.min.css")
  )
}

sc_report_tabset_panel <- function(...) {
  tabset <- sc_report_build_tabset(list(...))
  htmltools::div(class = "tabbable", tabset$nav_list, tabset$content)
}

sc_report_build_tabset <- function(tabs) {
  tabset_id <- "covr"
  tabs <- lapply(seq_len(length(tabs)), sc_report_build_tab_item, tabs = tabs, tabset_id = tabset_id)
  list(
    nav_list = sc_report_ul(
      class = "nav nav-tabs",
      `data-tabsetid` = tabset_id,
      lapply(tabs, "[[", 1)
    ),
    content = htmltools::div(
      class = "tab-content",
      `data-tabsetid` = tabset_id,
      lapply(tabs, "[[", 2)
    )
  )
}

sc_report_build_tab_item <- function(i, tabs, tabset_id) {
  div_tag <- tabs[[i]]
  tab_id <- paste("tab", tabset_id, i, sep = "-")
  li_tag <- sc_report_li(
    htmltools::a(
      href = paste0("#", tab_id),
      `data-toggle` = "tab",
      `data-value` = div_tag$attribs$`data-value`,
      div_tag$attribs$title
    )
  )
  if (i == 1) {
    li_tag$attribs$class <- "active"
    div_tag$attribs$class <- paste(div_tag$attribs$class, "active")
  }
  div_tag$attribs$id <- tab_id

  list(li_tag = li_tag, div_tag = div_tag)
}

sc_report_li <- function(...) htmltools::tag("li", list(...))
sc_report_ul <- function(...) htmltools::tag("ul", list(...))

sc_report_add_highlight <- function(x = list()) {
  highlight <- htmltools::htmlDependency(
    "highlight.js", "6.2",
    system.file(package = "covr", "www/shared/highlight.js"),
    script = "highlight.pack.js",
    stylesheet = "rstudio.css"
  )

  htmltools::attachDependencies(x, c(htmltools::htmlDependencies(x), list(highlight)))
}
