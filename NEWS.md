# shiny.cov 0.0.0.9000

Initial development version.

## Features

* `setup()`/`collect()`/`report()`/`ui_report()`/`cleanup()` for
  instrumenting a Shiny app, running it under shinytest2, Cypress, or
  Playwright, and collecting coverage.
* Real branch-level R line coverage for code loaded through `source()`
  **and** `box::use()` -- `{box}` and rhino apps are fully supported, not
  just plain `app.R`/`ui.R`+`server.R` apps. The AST-tracing/counting
  engine itself (`R/trace.R`) is ported in-tree from `{covr}` (MIT
  licensed, same as this package) rather than called via `covr:::` at
  runtime, so that engine no longer depends on covr's internal, unexported
  tracing API staying stable across versions, and the counter-swap
  mechanism needs no namespace-locking tricks -- it mutates shiny.cov's
  own state. `{covr}` is still a dependency: the result is a standard covr
  coverage object, so `covr::percent_coverage()`, `covr::report()`, and
  `covr::to_cobertura()` all work on it and it stays fully compatible
  with CI coverage services.
* UI interaction coverage (which inputs/outputs/tabs/conditional panels
  were actually exercised) is merged directly into the same `covr`
  coverage object as R line coverage, so `covr::percent_coverage()`,
  `covr::report()`, and `covr::to_cobertura()` all reflect server logic
  and UI interaction as a single number.
* UI discovery is framework-agnostic: inputs/outputs are found by asking
  the live browser's own `Shiny.inputBindings`/`Shiny.outputBindings`
  registry what's actually bound, not by guessing from CSS classes --
  works for any widget library (verified against `shinyWidgets`, not just
  base Shiny), including custom `Shiny.inputBindings.register()` calls,
  with zero special-casing.
* Real `shiny::moduleServer()`-based module-boundary detection, and a
  source-line attribution fallback (via `htmltools::tag()`/
  `tagAppendAttributes()`) for computed element ids that have no literal
  string to find in the source.
* `shiny.cov::AppDriver`, a `shinytest2::AppDriver` subclass that
  automatically logs UI interactions for the coverage merge above.
* A companion `shiny.cov-cypress` npm package for Cypress-driven tests:
  explicit interaction logging via `cy.shinyCovInteract()`, and a
  `cy.shinyCovDiscoverManifest()` command mirroring the R side's browser
  registry discovery. Automatic logging via overridden
  `cy.click()`/`cy.type()`/etc. commands is deliberately not provided:
  chaining a `cy.task()` call into an overridden actionability command is
  unreliable in a real Cypress browser run. Explicit logging is one extra
  line per interaction and is reliable. The `shinycov-start-and-test` CLI
  is not included -- the example projects use `cross-env` directly.
* A companion `shiny.cov-playwright` npm package for Playwright-driven
  tests: a `test`/`expect` pair that transparently wraps Playwright's
  `page`/`Locator` objects in a `Proxy`, so every recognized interaction
  -- including calls made through `getByRole()`/`getByLabel()`/`getByText()`
  and interactions inside a `page.frameLocator(...)` -- is logged
  automatically, with no explicit logging call needed per interaction.
  Manifest discovery runs the same browser-side script the shinytest2 and
  Cypress adapters use. There is no separate server-launch module; the app
  is started through Playwright's own `webServer` config.
* A companion `shiny.cov` Python package (`shiny.cov-py` on GitHub, a pytest plugin) bringing the
  same blended server-plus-UI coverage model to Shiny for Python apps
  tested with Playwright: server-side line coverage via `coverage.py`,
  UI discovery and interaction logging instrumented onto py-shiny's own
  `shiny.playwright.controller` classes, and the same browser-side
  discovery script and manifest-merge algorithm shared with the R and
  Playwright adapters.
* Coverage is saved periodically in the background, not just on graceful
  shutdown -- shutdown hooks (`onStop()`/`.Last()`) are best-effort and
  don't always fire reliably within the test framework's grace period.
  Saves merge with whatever's already on disk
  rather than overwriting it, so multiple concurrent `AppDriver` sessions
  against the same already-set-up app don't clobber each other's coverage.
* `setup()` refuses to run if it finds a leftover `*.shinycov_backup`
  from a previously-interrupted `setup()` call, rather than risking a
  double-wrap or silently doing nothing useful.
