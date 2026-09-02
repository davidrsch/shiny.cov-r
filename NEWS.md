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
  runtime -- shiny.cov no longer depends on covr's internal, unexported
  API remaining stable across versions, and the counter-swap mechanism no
  longer needs any namespace-locking tricks at all, since it's mutating
  its own state rather than reaching into another package's namespace.
  `{covr}` itself is still a real dependency -- `collect()`/`report()`
  build their output through its actual public API
  (`covr::percent_coverage()`, `covr::report()`, `covr::to_cobertura()`),
  so results stay fully compatible with it and CI coverage services.
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
  registry discovery. There is no automatic logging via overridden
  `cy.click()`/`cy.type()`/etc. commands -- every variation of that was
  tried and found fragile in ways only a real Cypress browser run exposed
  (mocked unit tests, which resolve `cy.task()` synchronously and don't
  exercise Cypress's real command queue or actionability-retry internals,
  couldn't catch any of it): an unreturned `cy.task()` call throws
  "Cypress detected that you returned a promise from a command while also
  invoking one or more cy commands in that promise"; chaining it before
  the real command breaks `type()`/`click()`'s own actionability retry
  ("Cannot read properties of null (reading 'length')"); chaining it
  after still threw the first error against a real app, consistently,
  across a full clean reinstall and every browser cache cleared. This
  matches a long-documented, never fully resolved class of problem in
  Cypress's own issue tracker for any plugin author overwriting an
  actionability command. Explicit logging is one extra line per
  interaction and has been reliable in every real run. The
  `shinycov-start-and-test` CLI has been removed -- neither real example
  project built against this package ended up using it (both use
  `cross-env` directly, which turned out simpler), so it was unnecessary
  surface area rather than genuine value.
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
