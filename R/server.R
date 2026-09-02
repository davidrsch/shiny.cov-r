# Process launch for the Cypress and Playwright adapters.
#
# Both adapters converge on this single R entry point:
#   * shiny.cov-playwright's `webServer.command` invokes it directly
#     (`Rscript -e "shiny.cov:::run_covr_server()"`), running in the R
#     process Playwright's `webServer` itself spawns.
#   * shiny.cov-cypress's `server.js` spawns the same command as its child
#     process (see that file's header), so the two adapters share one copy
#     of the env-var parsing and IPv4-loopback binding logic.

#' Run a Shiny app with coverage instrumentation, for the Cypress/Playwright adapters
#'
#' Reads the app directory and port from
#' `SHINYCOV_SERVER_APP_DIR`/`SHINYCOV_SERVER_PORT`. Playwright supplies
#' these via the consuming project's `playwright.config.ts` `webServer.env`
#' block (see `shiny.cov-playwright`'s README for the exact config block);
#' Cypress supplies the same two variables from `shiny.cov-cypress`'s
#' `server.js`.
#'
#' Coverage instrumentation itself is activated the same way it already is
#' for every other adapter: [setup()] must have already run against
#' `app_dir` (writing the `app.R` wrapper that checks `SHINYCOV_OUTPUT` at
#' source time), and `SHINYCOV_OUTPUT` must already be set in the
#' environment this function runs in. That variable is just one more entry
#' in the same `webServer.env` block (or `server.js` env), so it's already
#' present by the time `shiny::runApp()` below sources the wrapped `app.R`
#' -- this function doesn't call [setup()] and doesn't set it itself.
#' (`R_COVR` is not read; it's kept only for compatibility with older
#' shiny.cov releases.)
#'
#' @return Never returns under normal operation -- `shiny::runApp()` blocks
#'   until the server is stopped (e.g. by Playwright's `webServer`
#'   `gracefulShutdown`).
#' @keywords internal
run_covr_server <- function() {
  app_dir <- Sys.getenv("SHINYCOV_SERVER_APP_DIR", unset = "")
  port_raw <- Sys.getenv("SHINYCOV_SERVER_PORT", unset = "")

  if (!nzchar(app_dir)) {
    stop(
      "shiny.cov: SHINYCOV_SERVER_APP_DIR is not set. run_covr_server() ",
      "is meant to be invoked via Playwright's webServer config or ",
      "shiny.cov-cypress's server.js, both of which set it -- see the ",
      "respective README for the expected env block."
    )
  }
  port <- suppressWarnings(as.integer(port_raw))
  if (!nzchar(port_raw) || is.na(port)) {
    stop(
      "shiny.cov: SHINYCOV_SERVER_PORT is not set to a valid integer ",
      "(got ", sQuote(port_raw), "). See the note on SHINYCOV_SERVER_APP_DIR ",
      "above."
    )
  }

  # 127.0.0.1, not "localhost": mirrors shiny.cov-cypress/src/server.js's
  # readiness-poll rationale -- Node (and thus a Playwright webServer `url`
  # poll of e.g. http://127.0.0.1:<port>) can resolve "localhost" to the
  # IPv6 loopback first, which this never binds.
  shiny::runApp(appDir = app_dir, port = port, host = "127.0.0.1")
}
