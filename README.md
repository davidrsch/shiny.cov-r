# shiny.cov

UI and server coverage for Shiny apps tested with shinytest2, [Cypress](https://github.com/daviddrsch/shiny.cov-cypress), or [Playwright](https://github.com/daviddrsch/shiny.cov-playwright). shinytest2 support is built directly into this package; Cypress and Playwright each need their own companion npm package installed alongside it (linked above) -- neither works without this R package also installed, since the R side does the actual code instrumentation and coverage collection.

## Installation

```r
# From GitHub (once published)
remotes::install_github("daviddrsch/shiny.cov-r")
```

## Quick start

```r
library(shiny.cov)

# 1. Set up coverage instrumentation
shiny.cov::setup("my-shiny-app")

# 2. Run your tests
shinytest2::test_app("my-shiny-app/tests/shinytest2")

# 3. Collect coverage
cov <- shiny.cov::collect("my-shiny-app")

# 4. View the report
covr::report(cov)

# 5. Clean up
shiny.cov::cleanup("my-shiny-app")
```

## How it works

`shiny.cov::setup()` renames the app's entry point (`app.R`, or the `ui.R`+`server.R` pair) to a backup and writes a wrapper in its place that loads a bootstrap script before running the original code. The bootstrap:

- Instruments every file loaded via `source()`, and every module loaded via `box::use()` (so `{box}`/rhino apps are covered too), with real branch-level AST instrumentation via an in-tree tracer ported from `{covr}` (see `R/trace.R`) rather than calling `covr:::` internals at runtime -- not just "did this file load," but which branches actually ran, without depending on covr's unexported internals staying stable across versions.
- Discovers the UI manifest (inputs, outputs, tabs, conditional panels) from a live browser, by asking Shiny's own `Shiny.inputBindings`/`Shiny.outputBindings` registry what's actually bound on the page -- this works for any widget library (`shinyWidgets`, `shiny.fluent`, ...) without `shiny.cov` needing to know it exists, and regardless of whether `ui` is a static object or a function (as it always is for `{box}`/rhino apps).
- Merges UI interaction coverage directly into the same coverage object as R line coverage: an input/output that was never interacted with shows up as uncovered on its own source line, the same way an untested branch does. There's one coverage number, not two.
- Persists counters to `coverage.rds` on graceful shutdown, and also periodically in the background -- graceful shutdown isn't fully reliable across test frameworks/platforms, so an abrupt kill loses at most a couple of seconds of the most recent interaction instead of the whole session.

For Cypress, which starts the Shiny process via a plain shell command instead of `callr`, `setup()` also injects a `.Rprofile` snippet as a fallback so the bootstrap still activates.

## API

| Function | Purpose |
|---|---|
| `setup(app_dir)` | Instrument app, write bootstrap, set env vars |
| `collect(app_dir)` | Read coverage.rds, return a `covr` coverage object with UI coverage merged in |
| `covr_r(app_dir, ...)` | One call: `setup()`, run tests via `shinytest2::test_app()`, `collect()`, `cleanup()` -- returns the coverage object |
| `report(cov)` | Generate the combined HTML report (delegates to `covr::report()`) plus a per-element UI breakdown |
| `ui_report(cov)` | Per-element UI interaction breakdown on its own |
| `cleanup(app_dir)` | Remove temp files, restore the original entry point and `.Rprofile` |
| `AppDriver` | `shinytest2::AppDriver` subclass that automatically logs UI interactions |

## License

MIT © David Díaz
