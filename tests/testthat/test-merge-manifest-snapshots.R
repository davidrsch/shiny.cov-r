# Cross-language equivalence: merge_manifest_snapshots() (R) and
# mergeManifests() (shiny.cov-cypress/src/plugin.js) are independent
# hand-written implementations of the same algorithm. Both are asserted
# here against the same fixture's `expected` value rather than against each
# other, so agreement doesn't depend on a runtime cross-language bridge --
# see shiny.cov-cypress/test/merge-manifest-equivalence.test.js for the JS
# side of this same fixture.

#' Sort a merged manifest's elements/arrays into a canonical order
#'
#' Element order within `inputs`/`outputs` is not semantically meaningful --
#' `merge_manifest_snapshots()` builds its by-id lookup as an R list, while
#' the JS implementation uses a plain object whose key iteration order
#' treats integer-like string keys specially (ascending numeric, ahead of
#' insertion order). The fixture's numeric-id case exists to exercise that
#' difference, so both languages' tests must normalize by sorting before
#' comparing rather than relying on array order.
normalize_manifest <- function(manifest) {
  sort_by_id <- function(els) {
    if (is.null(els) || length(els) == 0) return(list())
    ids <- vapply(els, function(el) as.character(el$id %||% ""), character(1))
    els[order(ids)]
  }
  sort_chr <- function(v) sort(as.character(unlist(v)))

  list(
    inputs      = sort_by_id(manifest$inputs),
    outputs     = sort_by_id(manifest$outputs),
    tabs        = sort_chr(manifest$tabs),
    conditional = sort_chr(manifest$conditional),
    modules     = sort_chr(manifest$modules)
  )
}

test_that("merge_manifest_snapshots() matches the shared R/JS equivalence fixture", {
  fixture <- jsonlite::fromJSON(
    test_path("fixtures", "manifest-merge-cases.json"),
    simplifyVector = FALSE
  )

  for (case in fixture$cases) {
    actual <- merge_manifest_snapshots(case$old, case$new)
    expect_equal(
      normalize_manifest(actual),
      normalize_manifest(case$expected),
      info = case$name
    )
  }
})

# ---- N-way reduce over a snapshot array (the shape a per-worker adapter
# like the Playwright one writes to manifest-snapshots.json) ----

#' A minimal manifest snapshot for the reduce tests below.
snapshot <- function(inputs = list(), outputs = list(), tabs = character(),
                      conditional = character(), modules = character()) {
  list(inputs = inputs, outputs = outputs, tabs = tabs,
       conditional = conditional, modules = modules)
}

three_worker_snapshots <- list(
  snapshot(
    inputs = list(list(id = "picker_1", type = "shiny.selectInput", label = "")),
    tabs = "Main", conditional = "panel_a"
  ),
  snapshot(
    inputs = list(
      list(id = "picker_1", type = "shinyWidgets.pickerInput", label = "Picker 1"),
      list(id = "only_worker_2", type = "shiny.textInput", label = "Only worker 2")
    ),
    tabs = "Settings", modules = "mod1"
  ),
  snapshot(
    inputs = list(list(id = "picker_1", type = "shiny.selectInput", label = "")),
    outputs = list(list(id = "out", type = "shiny.textOutput", label = "")),
    tabs = "About", conditional = "panel_b", modules = c("mod1", "mod2")
  )
)

test_that("Reduce(merge_manifest_snapshots, ...) over 3+ snapshots matches a manual pairwise reduce", {
  actual <- Reduce(merge_manifest_snapshots, three_worker_snapshots)
  manual <- merge_manifest_snapshots(
    merge_manifest_snapshots(three_worker_snapshots[[1]], three_worker_snapshots[[2]]),
    three_worker_snapshots[[3]]
  )

  expect_equal(normalize_manifest(actual), normalize_manifest(manual))

  picker <- Filter(function(x) x$id == "picker_1", actual$inputs)[[1]]
  expect_equal(picker$type, "shinyWidgets.pickerInput")
  expect_equal(picker$label, "Picker 1")
  expect_setequal(unlist(actual$tabs), c("Main", "Settings", "About"))
  expect_setequal(unlist(actual$modules), c("mod1", "mod2"))
})

test_that("Reduce(merge_manifest_snapshots, ...) is order-independent when a specific type/label appears in only one snapshot", {
  # Playwright's worker completion order is nondeterministic in production,
  # so the merged result can't depend on which order the snapshots happen
  # to land in this array. "picker_1"'s specific type/label sit in the
  # *middle* snapshot on purpose (not first or last) -- a reduce that were
  # biased toward early or late elements instead of genuinely
  # order-independent would still pass a test that only ever put the
  # specific value first or last.
  orderings <- list(
    c(1L, 2L, 3L),
    c(3L, 2L, 1L),
    c(2L, 3L, 1L),
    c(3L, 1L, 2L)
  )

  merged <- lapply(orderings, function(idx) {
    normalize_manifest(Reduce(merge_manifest_snapshots, three_worker_snapshots[idx]))
  })

  for (m in merged[-1]) {
    expect_equal(m, merged[[1]])
  }

  picker <- Filter(function(x) x$id == "picker_1", merged[[1]]$inputs)[[1]]
  expect_equal(picker$type, "shinyWidgets.pickerInput")
  expect_equal(picker$label, "Picker 1")
})

test_that("order-independence does not extend to two snapshots disagreeing on which specific type an id has", {
  # A documented limitation, not a bug this change needs to fix:
  # merge_manifest_snapshots()'s more-specific-type-wins rule only knows how
  # to prefer a specific type over a base ("shiny."-prefixed or empty) one --
  # it has no way to arbitrate between two candidates that are *both*
  # specific. Whichever snapshot is merged in first keeps its type, so the
  # reduce is genuinely order-dependent in this case. This doesn't arise
  # from how a single widget's own DOM degrades over interaction (the
  # scenario the merge algorithm exists for -- see merge_manifest_snapshots()'s
  # docstring), only from two snapshots asserting flatly contradictory
  # information about the same id.
  conflicting <- list(
    snapshot(inputs = list(list(id = "x", type = "widgetA.thing", label = ""))),
    snapshot(inputs = list(list(id = "x", type = "widgetB.thing", label = "")))
  )

  forward  <- Reduce(merge_manifest_snapshots, conflicting)
  backward <- Reduce(merge_manifest_snapshots, rev(conflicting))

  expect_equal(forward$inputs[[1]]$type, "widgetA.thing")
  expect_equal(backward$inputs[[1]]$type, "widgetB.thing")
})

test_that("Reduce() over a single-element snapshot array returns that snapshot unchanged", {
  only <- three_worker_snapshots[[2]]
  expect_equal(Reduce(merge_manifest_snapshots, list(only)), only)
})

test_that("Reduce() over an empty snapshot array returns NULL", {
  expect_null(Reduce(merge_manifest_snapshots, list()))
})
