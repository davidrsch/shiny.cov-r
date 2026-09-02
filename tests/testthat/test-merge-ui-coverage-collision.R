# Regression tests for merge_ui_coverage()'s handling of two srcref-collision
# scenarios: a silently-lost untested signal for single-line widget
# statements, and a `message()` when two different elements share one
# physical source line.
#
# See the docstring above merge_ui_coverage() in R/collect.R for the full
# story: covr:::merge_values() (used internally by
# covr::percent_coverage()/covr::report()) sums entries whose full 8-number
# srcref tuple (+ filename + functions) is byte-for-byte identical, *before*
# the by-line min() reduction that's supposed to pull an untested line down
# to "uncovered" ever runs. The old code's synthetic "untested" entry used a
# srcref spanning the whole physical line -- for a UI element whose entire
# construction *is* a one-line statement, that's identical to the real covr
# entry covr's own tracer already produced for that statement, so the
# untested `value = 0` entry got summed into the real nonzero entry and
# silently vanished.

skip_if_no_covr()

#' Instrument, execute, and build a covr-style coverage list for a fixture
#' app, then hand-write manifest.json/interactions.json and run
#' merge_ui_coverage() on it -- the same shape of coverage object collect()
#' itself builds, without needing a real running Shiny session (there is
#' none here) or a real browser.
#'
#' @param fixture_name Directory under tests/testthat/fixtures/.
#' @param manifest A list with $inputs/$outputs (see manifest.json shape
#'   produced by discover-bindings.js / R/report.R).
#' @param interactions A list of `list(selector = "#id", action = "...")`
#'   entries (see interactions.json shape produced by R/shinytest2.R /
#'   shiny.cov-cypress).
#' @return The merged coverage object (`cov` after merge_ui_coverage()).
merge_fixture_coverage <- function(fixture_name, manifest, interactions) {
  app_dir <- create_temp_app(fixture_name)
  withr_cleanup <- function() cleanup_temp_app(app_dir)
  on.exit(withr_cleanup(), add = TRUE)

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  # Actually execute the top-level statements (as they would run once at
  # real app startup) so the real covr entries get value = 1, matching how
  # ui-construction code is always "covered" by ordinary execution -- the
  # whole reason merge_ui_coverage() exists.
  eval_instrumented(inst, env = new.env())
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", class(cov))

  jsonlite::write_json(manifest, file.path(shinycov_dir, "manifest.json"), auto_unbox = TRUE)
  jsonlite::write_json(interactions, file.path(shinycov_dir, "interactions.json"), auto_unbox = TRUE)

  merge_ui_coverage(cov, app_dir)
}

test_that("never-interacted single-line widget statement drops percent_coverage below 100", {
  manifest <- list(inputs = list(list(id = "go", type = "actionButton")), outputs = list())
  merged <- merge_fixture_coverage("single-line-widget-app", manifest, interactions = list())

  # The synthetic untested entry for "go" must be a distinct row, not
  # summed away into the real (always-executed) entry for its line.
  ui_key <- grep("^ui:go:", names(merged), value = TRUE)
  expect_gt(length(ui_key), 0)
  expect_equal(merged[[ui_key[[1]]]]$value, 0)

  expect_lt(covr::percent_coverage(merged), 100)
})

test_that("interacting with the single-line widget keeps percent_coverage at 100", {
  manifest <- list(inputs = list(list(id = "go", type = "actionButton")), outputs = list())
  interactions <- list(list(selector = "#go", action = "click"))
  merged <- merge_fixture_coverage("single-line-widget-app", manifest, interactions)

  ui_key <- grep("^ui:go:", names(merged), value = TRUE)
  expect_gt(length(ui_key), 0)
  expect_equal(merged[[ui_key[[1]]]]$value, 1)

  expect_equal(covr::percent_coverage(merged), 100)
})

test_that("multi-line UI construction still narrows uncovered marking to the element's own line only", {
  # "simple-app" builds its UI across many lines inside one fluidPage()
  # call: sliderInput("bins", ...) spans lines 6-7, actionButton("go", ...)
  # is line 8, both nested well inside the single, multi-line `ui <-
  # fluidPage(...)` statement. Only "go" goes untested here -- the
  # regression this guards is the enclosing statement's *entire* line range
  # being marked uncovered instead of just actionButton's own line.
  # distPlot deliberately left out of the manifest here: this test is only
  # about the sliderInput/actionButton narrowing behavior, and an untested
  # plotOutput would legitimately (and correctly) add its own uncovered
  # line, confounding the "only actionButton's line" assertion below.
  manifest <- list(
    inputs = list(
      list(id = "bins", type = "sliderInput"),
      list(id = "go", type = "actionButton")
    ),
    outputs = list()
  )
  interactions <- list(list(selector = "#bins", action = "set_inputs"))
  merged <- merge_fixture_coverage("simple-app", manifest, interactions)

  bins_key <- grep("^ui:bins:", names(merged), value = TRUE)
  go_key <- grep("^ui:go:", names(merged), value = TRUE)
  expect_gt(length(bins_key), 0)
  expect_gt(length(go_key), 0)
  expect_equal(merged[[bins_key[[1]]]]$value, 1)
  expect_equal(merged[[go_key[[1]]]]$value, 0)

  # The by-line tally is what percent_coverage()/report() actually use --
  # confirm only actionButton("go", ...)'s specific line drops to 0, not
  # every line inside the enclosing multi-line fluidPage() call. Scoped to
  # just that enclosing statement's own line range (not the whole file):
  # simple-app's server function has reactive code that -- under this
  # instrument-then-eval-once fixture harness, which never actually runs a
  # live reactive graph -- never executes either, and would otherwise show
  # as unrelated uncovered lines that have nothing to do with this
  # regression.
  by_line <- covr:::tally_coverage(merged, by = "line")
  go_line <- merged[[go_key[[1]]]]$srcref[[1]]

  real_entries <- merged[!grepl("^ui:", names(merged))]
  enclosing <- Filter(function(x) x$srcref[[1]] <= go_line && x$srcref[[3]] >= go_line, real_entries)
  expect_gt(length(enclosing), 0)
  range_first <- enclosing[[1]]$srcref[[1]]
  range_last <- enclosing[[1]]$srcref[[3]]
  expect_gt(range_last, range_first) # sanity check: genuinely multi-line

  in_range <- by_line[by_line$line >= range_first & by_line$line <= range_last, ]
  expect_equal(in_range$line[in_range$value == 0], go_line)

  expect_lt(covr::percent_coverage(merged), 100)
})

test_that("two elements resolving to the same physical line both keep their own per-entry value", {
  manifest <- list(
    inputs = list(
      list(id = "go", type = "actionButton"),
      list(id = "stop", type = "actionButton")
    ),
    outputs = list()
  )
  interactions <- list(list(selector = "#go", action = "click")) # "stop" left untested
  merged <- merge_fixture_coverage("collision-widgets-app", manifest, interactions)

  go_key <- grep("^ui:go:", names(merged), value = TRUE)
  stop_key <- grep("^ui:stop:", names(merged), value = TRUE)
  expect_gt(length(go_key), 0)
  expect_gt(length(stop_key), 0)

  # Per-entry values in `cov` itself are unaffected by the line-sharing --
  # this is what ui_report()'s per-element table (built from the same
  # hit_count/manifest data) can still disambiguate.
  expect_equal(merged[[go_key[[1]]]]$value, 1)
  expect_equal(merged[[stop_key[[1]]]]$value, 0)

  # But the two entries must NOT be byte-identical to each other, or
  # covr:::merge_values() would sum them (1 + 0 = 1) before the by-line
  # min() reduction runs, silently hiding "stop"'s untested status behind
  # a nonzero sum.
  expect_false(identical(
    as.numeric(merged[[go_key[[1]]]]$srcref),
    as.numeric(merged[[stop_key[[1]]]]$srcref)
  ))

  # The shared physical line can still only carry one verdict once reduced
  # to line granularity -- covr's own min() correctly makes that the
  # untested one, which is exactly the granularity limitation the
  # message() (tested below) exists to explain, not something further
  # fixable here.
  by_line <- covr:::tally_coverage(merged, by = "line")
  go_line <- merged[[go_key[[1]]]]$srcref[[1]]
  expect_equal(by_line$value[by_line$line == go_line], 0)
})

test_that("merge_ui_coverage() messages when two elements sharing a line disagree on tested status", {
  manifest <- list(
    inputs = list(
      list(id = "go", type = "actionButton"),
      list(id = "stop", type = "actionButton")
    ),
    outputs = list()
  )
  interactions <- list(list(selector = "#go", action = "click")) # "stop" left untested

  expect_message(
    merge_fixture_coverage("collision-widgets-app", manifest, interactions),
    "resolve to.*differ in tested status"
  )
})

test_that("no collision message when elements sharing a line agree on tested status", {
  manifest <- list(
    inputs = list(
      list(id = "go", type = "actionButton"),
      list(id = "stop", type = "actionButton")
    ),
    outputs = list()
  )
  # Both untested -- same status, nothing ambiguous to warn about.
  expect_no_message(
    merge_fixture_coverage("collision-widgets-app", manifest, interactions = list()),
    message = "differ in tested status"
  )
})

# ---- the same-line collision message must not fire on a false "shared
# line" claim caused by locate_by_srcref()'s pre-existing coarseness ----
#
# bootstrap.R's htmltools::tag()/tagAppendAttributes() hook attributes a
# computed/nested id (no srcref of its own -- a bare call argument, not its
# own block-member statement) to shinycov_last_srcref(): the srcref of
# whichever instrumented statement last ran, not necessarily the id's own
# call site. This is visible in the real e2e-app fixture (see test-e2e.R):
# the "counter" module's `textOutput(ns("total"))` (a bare tagList()
# argument, actually on line 14) gets attributed to line 12 -- the
# `sliderInput(ns(paste0("bin_", i)), ...)` statement inside the module's
# `lapply()` loop, which happens to be the last thing that ran. Without the
# suppression this test exercises, the collision message would then report
# "counter-bin_1, counter-bin_2, counter-total ... share [line 12] ...
# differ in tested status", which is misleading: counter-total's real
# location (line 14) shares nothing with the other two. counter-bin_1 and
# counter-bin_2 legitimately DO share line
# 12 (both are built by the same loop-body statement executed twice), so
# suppressing the message here is a conservative trade-off, not a targeted
# fix of only the false part -- see R/collect.R's merge_ui_coverage()
# docstring for why a general per-id "is this attribution trustworthy"
# signal isn't available at this level.
test_that("a fallback-located (srcref-attributed) id colliding on a line suppresses the same-line collision message", {
  app_dir <- create_temp_app("e2e-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  eval_instrumented(inst, env = new.env())
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", class(cov))

  # Pull the real srcref covr's own tracer produced for line 12 (the
  # sliderInput() statement) -- exactly what shinycov_last_srcref() would
  # have returned while that statement was executing, i.e. exactly what
  # the htmltools::tag() hook's fallback would have captured for
  # "counter-bin_1"/"counter-bin_2" (correctly) and, without the
  # suppression this test exercises, for "counter-total" too (incorrectly
  # -- its real location is line 14).
  bin_line_entries <- Filter(function(x) {
    inherits(x$srcref, "srcref") && x$srcref[[1]] == 12L
  }, cov)
  expect_gt(length(bin_line_entries), 0)
  line12_sr <- bin_line_entries[[1]]$srcref

  id_srcrefs <- list(
    `counter-bin_1` = line12_sr,
    `counter-bin_2` = line12_sr,
    `counter-total` = line12_sr # the misattribution under test
  )
  saveRDS(id_srcrefs, file.path(shinycov_dir, "id_srcrefs.rds"))

  manifest <- list(
    inputs = list(
      list(id = "counter-bin_1", type = "sliderInput"),
      list(id = "counter-bin_2", type = "sliderInput")
    ),
    outputs = list(list(id = "counter-total", type = "textOutput"))
  )
  # counter-bin_1 tested, counter-bin_2 and counter-total left untested --
  # tested status genuinely differs among the three ids that (per
  # id_srcrefs.rds above) all resolve to line 12.
  interactions <- list(list(selector = "#counter-bin_1", action = "set_inputs"))

  jsonlite::write_json(manifest, file.path(shinycov_dir, "manifest.json"), auto_unbox = TRUE)
  jsonlite::write_json(interactions, file.path(shinycov_dir, "interactions.json"), auto_unbox = TRUE)

  # None of these three ids has a literal string in the source to
  # text-search for (all computed via ns(paste0(...)) / ns("total")), so
  # every one of them is located purely via the srcref fallback -- exactly
  # the low-confidence case that would otherwise fire a false "elements
  # counter-bin_1, counter-bin_2, counter-total ... differ in tested
  # status" message.
  expect_no_message(
    merged <- merge_ui_coverage(cov, app_dir),
    message = "differ in tested status"
  )

  # The underlying per-id values are independent of the message above --
  # counter-total is still (mis)located at line 12 and still correctly
  # counted as untested; suppressing a false "shared line" claim in the
  # message is purely about the message, not about the location itself.
  total_key <- grep("^ui:counter-total:", names(merged), value = TRUE)
  expect_gt(length(total_key), 0)
  expect_equal(merged[[total_key[[1]]]]$value, 0)
})

# ---- merge_ui_coverage() defensive hardening against a malformed
# selector in interactions.json ----
#
# A logged NULL selector serializes as a bare empty JSON object
# (`"selector": {}`), not JSON null. jsonlite parses that back as a
# non-NULL empty *list*, so a naive `sel <- x$selector %||% ""` doesn't
# catch it: `grepl("^#", list())` returns `logical(0)`, and
# `if (logical(0) && ...)` throws "missing value where TRUE/FALSE needed"
# -- crashing collect() for the *entire app*, not just discarding that one
# malformed entry. Degrading any non-character/wrong-length/NA selector to
# "" (skip this entry) guards against any cause of a malformed selector,
# not just the specific click(output=...) case that can produce one (which
# is separately regression-tested against a real AppDriver in
# test-shinytest2.R).
test_that("a malformed (empty-list) selector in interactions.json doesn't crash merge_ui_coverage()", {
  app_dir <- create_temp_app("single-line-widget-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)

  # Minimal manifest -- empty is fine; the crash site (the hit_count loop)
  # runs before merge_ui_coverage() ever looks at manifest elements.
  writeLines('{"inputs": [], "outputs": []}', file.path(shinycov_dir, "manifest.json"))
  # Hand-written to exactly reproduce the on-disk shape a malformed logged
  # selector produces: a bare `{}` for "selector", which jsonlite::fromJSON
  # (simplifyVector = FALSE) parses as list(), not NULL.
  writeLines(
    '[{"selector": {}, "action": "click", "timestamp": 1}]',
    file.path(shinycov_dir, "interactions.json")
  )

  cov <- list()
  class(cov) <- c("coverage", "list")

  expect_no_error(merged <- merge_ui_coverage(cov, app_dir))
  expect_identical(merged, cov)
})

test_that("a malformed selector entry is skipped, not counted, while valid sibling entries still are", {
  manifest <- list(inputs = list(list(id = "go", type = "actionButton")), outputs = list())
  app_dir <- create_temp_app("single-line-widget-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)
  jsonlite::write_json(manifest, file.path(shinycov_dir, "manifest.json"), auto_unbox = TRUE)
  # One malformed entry (would crash an unhardened merge_ui_coverage())
  # alongside one valid, real "go" click -- the malformed one must be
  # silently skipped, and must not prevent the valid entry from still
  # being counted as a hit.
  writeLines(
    '[{"selector": {}, "action": "click", "timestamp": 1},
      {"selector": "#go", "action": "click", "timestamp": 2}]',
    file.path(shinycov_dir, "interactions.json")
  )

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  eval_instrumented(inst, env = new.env())
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", class(cov))

  merged <- expect_no_error(merge_ui_coverage(cov, app_dir))
  ui_key <- grep("^ui:go:", names(merged), value = TRUE)
  expect_gt(length(ui_key), 0)
  expect_equal(merged[[ui_key[[1]]]]$value, 1)
})

# ---- merge_ui_coverage() defensive hardening against a malformed
# `action` in interactions.json ----
#
# The `sel` (`selector` field) hardening above guards one half of the
# same `if`, but the other half (`(x$action %||% "") %in% allowed_actions`)
# needs the identical guard: `x$action` isn't always NULL or a genuine
# length-1 character string. A malformed `"action": {}` -- the same
# on-disk shape jsonlite produces for any logged NULL -- sails through
# `%||%` unchanged (it's not NULL) and makes `list() %in% allowed_actions`
# return `logical(0)`, which throws "missing value where TRUE/FALSE
# needed" from the enclosing `&&`, crashing collect() for the whole app.
# Mirrors the selector-hardening tests above exactly, but on `action`
# instead of `selector`.
test_that("a malformed (empty-list) action in interactions.json doesn't crash merge_ui_coverage()", {
  app_dir <- create_temp_app("single-line-widget-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)

  writeLines('{"inputs": [], "outputs": []}', file.path(shinycov_dir, "manifest.json"))
  # Hand-written to exactly reproduce the on-disk shape a malformed logged
  # action would produce: a bare `{}` for "action", which jsonlite::fromJSON
  # (simplifyVector = FALSE) parses as list(), not NULL -- a valid selector
  # is required here so the `grepl("^#", sel)` half of the `&&` is TRUE and
  # execution actually reaches the `act %in% allowed_actions` half this
  # test is about.
  writeLines(
    '[{"selector": "#go", "action": {}, "timestamp": 1}]',
    file.path(shinycov_dir, "interactions.json")
  )

  cov <- list()
  class(cov) <- c("coverage", "list")

  expect_no_error(merged <- merge_ui_coverage(cov, app_dir))
  expect_identical(merged, cov)
})

test_that("a malformed action entry is skipped, not counted, while valid sibling entries still are", {
  manifest <- list(inputs = list(list(id = "go", type = "actionButton")), outputs = list())
  app_dir <- create_temp_app("single-line-widget-app")
  on.exit(cleanup_temp_app(app_dir), add = TRUE)

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)
  jsonlite::write_json(manifest, file.path(shinycov_dir, "manifest.json"), auto_unbox = TRUE)
  # One malformed-action entry (would crash an unhardened
  # merge_ui_coverage()) alongside one valid, real "go" click -- the
  # malformed one must be silently skipped, and must not prevent the valid
  # entry from still being counted as a hit.
  writeLines(
    '[{"selector": "#go", "action": {}, "timestamp": 1},
      {"selector": "#go", "action": "click", "timestamp": 2}]',
    file.path(shinycov_dir, "interactions.json")
  )

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  eval_instrumented(inst, env = new.env())
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", class(cov))

  merged <- expect_no_error(merge_ui_coverage(cov, app_dir))
  ui_key <- grep("^ui:go:", names(merged), value = TRUE)
  expect_gt(length(ui_key), 0)
  expect_equal(merged[[ui_key[[1]]]]$value, 1)
})

# ---- locate_by_text() escapes regex metacharacters in manifest ids ----
#
# regex_escape() itself is unit-tested against every metacharacter in
# test-collect.R; this is the integration-level regression: a real manifest
# id containing regex metacharacters must not throw "invalid regular
# expression" (e.g. unbalanced "(" from "go.now(1)+") and must still locate
# the right line via an ordinary literal-text match.
test_that("a manifest id containing regex metacharacters doesn't crash merge_ui_coverage()", {
  app_dir <- file.path(tempdir(), "regex-metachar-app")
  if (dir.exists(app_dir)) unlink(app_dir, recursive = TRUE)
  dir.create(app_dir, recursive = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)

  # "." "(" ")" "+" are all regex metacharacters; "(" "+" unescaped and
  # unbalanced is exactly what used to throw a raw "invalid regular
  # expression" error out of grepl(), aborting collect() for the whole app.
  weird_id <- "go.now(1)+"
  writeLines(
    c(
      "library(shiny)",
      sprintf('ui <- actionButton("%s", "Go")', weird_id),
      "server <- function(input, output, session) {",
      "}",
      "shinyApp(ui = ui, server = server)"
    ),
    file.path(app_dir, "app.R")
  )

  shinycov_dir <- file.path(app_dir, ".shiny.cov")
  dir.create(shinycov_dir, showWarnings = FALSE)
  manifest <- list(inputs = list(list(id = weird_id, type = "actionButton")), outputs = list())
  jsonlite::write_json(manifest, file.path(shinycov_dir, "manifest.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(), file.path(shinycov_dir, "interactions.json"), auto_unbox = TRUE)

  app_path <- normalizePath(file.path(app_dir, "app.R"), winslash = "/", mustWork = TRUE)
  inst <- instrument_file(app_path)
  eval_instrumented(inst, env = new.env())
  cov <- build_coverage(inst$counters)
  class(cov) <- c("coverage", class(cov))

  merged <- expect_no_error(merge_ui_coverage(cov, app_dir))

  # Located correctly (line 2, the actionButton() statement) and counted as
  # untested (no interactions were logged for it).
  ui_key <- grep(weird_id, names(merged), fixed = TRUE, value = TRUE)
  expect_gt(length(ui_key), 0)
  expect_equal(merged[[ui_key[[1]]]]$value, 0)
  expect_equal(merged[[ui_key[[1]]]]$srcref[[1]], 2)
})

# ---- merge_ui_coverage() messages about unlocatable manifest elements ----
#
# Previously, `if (is.null(loc)) next` silently dropped any manifest
# element neither locate_by_text() nor locate_by_srcref() could place --
# unlike the "no manifest found"/"no interaction log found" cases right
# above it in the same function, which both message(). An unlocatable
# element is just as much a gap, and silently excluding it biases the
# blended coverage percentage toward looking more complete than the
# manifest actually reflects.
test_that("merge_ui_coverage() messages when it can't locate a source line for a manifest element", {
  manifest <- list(
    inputs = list(list(id = "totally-nonexistent-id", type = "actionButton")),
    outputs = list()
  )
  expect_message(
    merged <- merge_fixture_coverage("single-line-widget-app", manifest, interactions = list()),
    "could not locate a source line for 1 UI element"
  )
  # No synthetic entry should have been fabricated for an id that was never
  # actually located.
  expect_equal(length(grep("totally-nonexistent-id", names(merged), fixed = TRUE)), 0)
})

test_that("no unlocatable-element message when every manifest element is located", {
  manifest <- list(inputs = list(list(id = "go", type = "actionButton")), outputs = list())
  expect_no_message(
    merge_fixture_coverage("single-line-widget-app", manifest, interactions = list()),
    message = "could not locate a source line"
  )
})

# ---- merge_ui_coverage() messages instead of silently swallowing a real
# JSON parse error (as opposed to "file doesn't exist", which is already
# messaged separately) for both manifest.json and interactions.json ----
test_that("merge_ui_coverage() messages when manifest.json exists but fails to parse", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  writeLines("{ not valid json", file.path(app_dir, ".shiny.cov", "manifest.json"))

  expect_message(
    result <- merge_ui_coverage(list(), app_dir),
    "manifest\\.json.*could not be parsed"
  )
  expect_identical(result, list())
})

test_that("merge_ui_coverage() messages when interactions.json exists but fails to parse", {
  app_dir <- create_temp_app("simple-app")
  on.exit(cleanup_temp_app(app_dir))

  dir.create(file.path(app_dir, ".shiny.cov"), showWarnings = FALSE)
  writeLines('{"inputs": [], "outputs": []}', file.path(app_dir, ".shiny.cov", "manifest.json"))
  writeLines("{ not valid json", file.path(app_dir, ".shiny.cov", "interactions.json"))

  expect_message(
    result <- merge_ui_coverage(list(), app_dir),
    "interactions\\.json.*could not be parsed"
  )
  expect_identical(result, list())
})
