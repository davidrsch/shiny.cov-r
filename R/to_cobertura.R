#' Export coverage to a Cobertura XML file
#'
#' Writes a Cobertura XML report for a [collect()] coverage object, with a
#' single `<line>` entry per physical source line. Unlike
#' [covr::to_cobertura()], which writes the same line once per enclosing
#' function (its method-level `<lines>` blocks) and is therefore misread by
#' some coverage services (e.g. codecov) as 100% coverage, this collapses
#' the tally to one row per `file:line` using the minimum hit count, so
#' uncovered lines are reported correctly.
#'
#' @param cov A coverage object returned by [collect()].
#' @param filename Path to write the Cobertura XML to.
#'
#' @return Invisibly returns `filename`.
#' @export
to_cobertura <- function(cov, filename = "cobertura.xml") {
  df <- covr::tally_coverage(cov, by = "line")
  agg <- stats::aggregate(
    df$value,
    by = list(filename = df$filename, line = df$line),
    FUN = min
  )
  names(agg)[3] <- "value"
  agg <- agg[order(agg$filename, agg$line), ]

  files <- unique(agg$filename)
  n <- nrow(agg)
  covered <- sum(agg$value > 0)
  rate <- covered / n

  xml <- c('<?xml version="1.0" encoding="UTF-8"?>')
  xml <- c(xml, sprintf(
    '<coverage line-rate="%s" branch-rate="0" lines-covered="%d" lines-valid="%d" branches-covered="0" branches-valid="0" complexity="0" version="1.0" timestamp="%s">',
    rate, covered, n, format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ))
  xml <- c(xml, "  <sources></sources>", "  <packages>")
  xml <- c(xml, sprintf(
    '    <package name="shiny.cov" line-rate="%s" branch-rate="0" complexity="0">',
    rate
  ))
  xml <- c(xml, "      <classes>")

  for (f in files) {
    sub <- agg[agg$filename == f, ]
    f_covered <- sum(sub$value > 0)
    f_rate <- f_covered / nrow(sub)
    xml <- c(xml, sprintf(
      '        <class name="%s" filename="%s" line-rate="%s" branch-rate="0" complexity="0">',
      basename(f), f, f_rate
    ))
    xml <- c(xml, "          <methods/>", "          <lines>")
    for (j in seq_len(nrow(sub))) {
      xml <- c(xml, sprintf(
        '            <line number="%d" hits="%d" branch="false"/>',
        sub$line[j], sub$value[j]
      ))
    }
    xml <- c(xml, "          </lines>", "        </class>")
  }

  xml <- c(xml, "      </classes>", "    </package>", "  </packages>", "</coverage>")
  writeLines(xml, filename)
  invisible(filename)
}
