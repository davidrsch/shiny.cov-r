# shiny.cov bootstrap — auto-generated, do not edit
# Sourced by the app wrapper created by shiny.cov::setup().
#
# This script deliberately delegates all instrumentation logic to the
# installed shiny.cov package (shiny.cov:::instrument_file(), and the
# tracing engine it and the box-module hook below both call into,
# R/trace.R) rather than reimplementing it inline -- requiring shiny.cov
# to be loadable in the child process avoids a second hand-written copy
# drifting out of sync with the real implementation.

output_file <- Sys.getenv("SHINYCOV_OUTPUT", unset = NA_character_)

# id_srcrefs.rds / modules.rds are app-level metadata (shared across test
# sources), so derive them from the untagged coverage path once, up front.
id_srcrefs_file <- sub("coverage\\.rds$", "id_srcrefs.rds", output_file)
modules_file <- sub("coverage\\.rds$", "modules.rds", output_file)

# When SHINYCOV_SOURCE is set (e.g. "shinytest2", "cypress", "playwright"),
# tag the coverage file so each test source accumulates its own counters
# rather than overwriting the others'; collect() then sums them.
shinycov_source <- Sys.getenv("SHINYCOV_SOURCE", unset = "")
if (nzchar(shinycov_source)) {
  output_file <- sub(
    "coverage\\.rds$",
    paste0("coverage.", shinycov_source, ".rds"),
    output_file
  )
}

if (!is.na(output_file) && nzchar(output_file)) {

# ---- 1. Counter environment ----
.shinycov_counters <- new.env(parent = emptyenv())

if (!requireNamespace("shiny.cov", quietly = TRUE)) {
  warning("shiny.cov: {shiny.cov} package not available in this R session. ",
          "Coverage disabled. It must be installed (or otherwise loadable ",
          "via requireNamespace()) in whatever R process runs the app, not ",
          "just in the parent session that called shiny.cov::setup().")
} else {

# ---- 2. Set the permanent, process-wide active counters ----
# shiny.cov owns its own tracing/counting engine (R/trace.R, ported from
# covr's -- see that file's header for why) -- {covr} itself isn't even
# needed in this child process anymore, only in whatever process later
# calls shiny.cov::collect()/report(). Setting the active counters is a
# plain accessor call now, not namespace surgery: shiny.cov is mutating
# its own state here, not reaching into another package's locked
# namespace, so there's nothing to unlock/lock.
tryCatch({
  shiny.cov:::shinycov_set_counters(.shinycov_counters)
}, error = function(e) {
  message("shiny.cov: could not set the active counters: ", conditionMessage(e))
})

# ---- 3. Helpers ----
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- 3a. box module instrumentation ----
# {box}-based apps (including rhino apps, whose root app.R is just
# rhino::app()) load every module file through box:::load_from_source(),
# not base::source() -- the source() override below never sees this code
# at all. The actual patch logic (surgically replacing
# box:::load_from_source()'s eval() statement with an instrumented
# version, with guards against {box} internal-API drift) lives in the
# installed package as shiny.cov:::install_box_hook() -- R/box_hook.R --
# rather than inline here, so it's a real, directly testable function
# instead of hand-duplicated text (see test-box-hook.R).
if (requireNamespace("box", quietly = TRUE)) {
  tryCatch({
    shiny.cov:::install_box_hook()
  }, error = function(e) {
    message("shiny.cov: could not instrument {box} module loading: ", conditionMessage(e))
  })
}

# ---- UI manifest ----
# Discovered from the live browser after the app has rendered, by asking
# Shiny's own Shiny.inputBindings/Shiny.outputBindings registry what's
# actually bound -- see inst/js/discover-bindings.js, run via
# shiny.cov::AppDriver$stop() (shinytest2) or
# cy.shinyCovDiscoverManifest() (Cypress, in shiny.cov-cypress/src/
# support.js). Both write the same .shiny.cov/manifest.json.

# ---- 3c. UI element id -> source line attribution ----
# merge_ui_coverage() (R/collect.R) locates each UI element's source line
# primarily via literal text search, which can't find *computed* ids (e.g.
# `sliderInput(paste0("bin_", i), ...)` inside a loop). As a fallback,
# patch htmltools::tag() -- the low-level constructor essentially all
# Shiny input/output HTML passes through, regardless of widget library --
# to record which instrumented statement was executing when a tag with an
# id was constructed (tag() itself carries no srcref of its own; R only
# attaches srcref to block-member statements, not nested call expressions).
# R/trace.R's shinycov_count() already records the srcref of whatever it
# last counted, read back here via shiny.cov:::shinycov_last_srcref().
#
# This attribution is only as fine-grained as the tracing engine's own
# instrumentation -- coarse for a flat `fluidPage(sliderInput(...), ...)`
# expression, but computed ids are almost always their own statement inside
# a loop/lapply body (a `{ }` block), which *does* get per-line srcrefs.
# That's why merge_ui_coverage() tries text search first and only falls
# back to this lookup.
.shinycov_id_srcrefs <- new.env(parent = emptyenv())

if (requireNamespace("htmltools", quietly = TRUE)) {
  tryCatch({
    ht_ns <- asNamespace("htmltools")
    orig_tag <- ht_ns$tag
    new_tag <- function(...) {
      result <- orig_tag(...)
      id <- result$attribs$id
      ref <- shiny.cov:::shinycov_last_srcref()
      if (!is.null(ref) && is.character(id) && length(id) == 1L && nzchar(id)) {
        assign(id, ref, envir = .shinycov_id_srcrefs)
      }
      result
    }
    environment(new_tag) <- list2env(
      list(
        orig_tag = orig_tag,
        .shinycov_id_srcrefs = .shinycov_id_srcrefs
      ),
      parent = ht_ns
    )
    unlockBinding("tag", ht_ns)
    assign("tag", new_tag, envir = ht_ns)
    lockBinding("tag", ht_ns)

    # shinyWidgets::pickerInput() builds its <select> via `tag("select",
    # ...)` with no `id` yet, then attaches `id` afterwards via
    # `tagAppendAttributes(tag = selectTag, id = inputId, ...)` -- a second,
    # separate htmltools call. The tag() patch alone never sees that id,
    # since it's added after tag() already returned. This build-bare-tag-
    # then-attach-id-post-hoc pattern isn't unique to shinyWidgets, so patch
    # tagAppendAttributes() too, the same way.
    orig_tag_append_attributes <- ht_ns$tagAppendAttributes
    new_tag_append_attributes <- function(tag, ...) {
      result <- orig_tag_append_attributes(tag, ...)
      id <- result$attribs$id
      ref <- shiny.cov:::shinycov_last_srcref()
      if (!is.null(ref) && is.character(id) && length(id) == 1L && nzchar(id)) {
        assign(id, ref, envir = .shinycov_id_srcrefs)
      }
      result
    }
    environment(new_tag_append_attributes) <- list2env(
      list(
        orig_tag_append_attributes = orig_tag_append_attributes,
        .shinycov_id_srcrefs = .shinycov_id_srcrefs
      ),
      parent = ht_ns
    )
    unlockBinding("tagAppendAttributes", ht_ns)
    assign("tagAppendAttributes", new_tag_append_attributes, envir = ht_ns)
    lockBinding("tagAppendAttributes", ht_ns)
  }, error = function(e) {
    message("shiny.cov: could not instrument htmltools::tag()/tagAppendAttributes() for source attribution: ", conditionMessage(e))
  })
}

# ---- 3d. Module namespace detection ----
# manifest$modules / each element's `module` field is populated by the
# discovery script purely by splitting ids on Shiny's `-`
# namespacing convention, which is only a guess -- it can't distinguish a
# real module boundary from a literal hyphen an app author chose to put in
# a plain top-level id. Hook shiny::moduleServer() (core Shiny, so this
# works no matter which UI framework/widget library sits on top of it) to
# record the actual namespace hierarchy as constructed at runtime, keyed by
# the same id prefix Shiny itself uses to route inputs to that module.
.shinycov_modules <- new.env(parent = emptyenv())

if (requireNamespace("shiny", quietly = TRUE) && is.function(shiny::moduleServer)) {
  tryCatch({
    shiny_ns <- asNamespace("shiny")
    orig_module_server <- shiny_ns$moduleServer
    new_module_server <- function(id, module, session = shiny::getDefaultReactiveDomain()) {
      # session here is the *parent* session (moduleServer() creates the
      # namespaced child session internally, below, via callModule()) --
      # its own $ns() already composes the full chain of enclosing module
      # prefixes, so applying it once more to this call's plain `id`
      # yields the correct fully-nested boundary prefix directly, the same
      # way shiny itself derives every nested module's namespace.
      ns_prefix <- tryCatch({
        if (!is.null(session) && is.function(session$ns)) session$ns(id) else id
      }, error = function(e) id)
      assign(ns_prefix, TRUE, envir = .shinycov_modules)
      orig_module_server(id, module, session)
    }
    environment(new_module_server) <- list2env(
      list(orig_module_server = orig_module_server, .shinycov_modules = .shinycov_modules),
      parent = shiny_ns
    )
    unlockBinding("moduleServer", shiny_ns)
    assign("moduleServer", new_module_server, envir = shiny_ns)
    lockBinding("moduleServer", shiny_ns)

    # Unlike htmltools::tag() (always called as `htmltools::tag()`, so
    # every caller resolves it dynamically via the namespace -- the patch
    # above is already sufficient), app code calls `moduleServer()`
    # *unqualified*, relying on `library(shiny)` search-path attachment.
    # The real shinytest2 launch path already attaches `package:shiny` to
    # search() before this hook ever runs (Shiny's own server machinery
    # needs shiny attached to serve the app at all), so that attached copy
    # is a frozen snapshot of moduleServer from attach time and does *not*
    # see this later namespace-level patch; an app's own top-level
    # `library(shiny)` call is then a no-op (already attached) and never
    # refreshes it either. Patch the attached copy directly too, if it
    # exists.
    if ("package:shiny" %in% search()) {
      attached_env <- as.environment("package:shiny")
      unlockBinding("moduleServer", attached_env)
      assign("moduleServer", new_module_server, envir = attached_env)
      lockBinding("moduleServer", attached_env)
    }
  }, error = function(e) {
    message("shiny.cov: could not instrument shiny::moduleServer() for module detection: ", conditionMessage(e))
  })
}

# ---- 4. Instrumentation + evaluation ----
# Evaluate instrumented expressions. Overrides source() in the eval
# environment so nested source() calls (e.g. source("R/helpers.R")) are
# also instrumented.
source_instrumented <- function(path, envir = .GlobalEnv) {
  inst <- shiny.cov:::instrument_file(path)

  # instrument_file() registers its counters into a fresh, file-local
  # environment and temporarily swaps the active counters to it, restoring
  # the previous binding (this session's permanent .shinycov_counters)
  # before returning. Merge its entries in so that count() calls embedded
  # in the instrumented expressions -- which may fire much later, e.g.
  # from reactive code triggered by a UI test -- land in the same
  # permanently-active counter environment.
  for (k in ls(inst$counters, all.names = TRUE)) {
    assign(k, get(k, envir = inst$counters), envir = .shinycov_counters)
  }

  already_overridden <- exists(".shinycov_source_active", envir = envir, inherits = FALSE)
  if (!already_overridden) {
    old_source <- envir[["source"]]
    assign(".shinycov_source_active", TRUE, envir = envir)
    envir$source <- function(file, ...) {
      resolved <- if (file.exists(file)) file
                  else file.path(dirname(path), file)
      source_instrumented(resolved, envir)
    }
    on.exit({
      if (is.null(old_source)) rm("source", envir = envir)
      else envir$source <- old_source
      rm(".shinycov_source_active", envir = envir)
    })
  }
  result <- NULL
  for (e in inst$exprs) result <- eval(e, envir = envir)

  # UI manifest is not built here -- see the "UI manifest" note above: it's
  # discovered from the live browser instead. Quiet because this fires once
  # per source()d/box::use()d file, which can be several times during a
  # single app's startup for a multi-file app.
  save_coverage(quiet = TRUE)
  invisible(result)
}

# ---- 5. Persistence ----
#' @param quiet If `TRUE`, don't print the "coverage saved" success
#'   message. Used for saves that fire routinely during a session (per-file
#'   loads, the periodic background save, per-session `onStop()`) so a
#'   long-running app's console isn't flooded with a message every ~2
#'   seconds. Save *failures* always print regardless of `quiet`.
save_coverage <- function(quiet = FALSE) {
  if (!exists(".shinycov_counters", where = .GlobalEnv, inherits = FALSE))
    return()
  counters <- as.list(.shinycov_counters)
  if (length(counters) == 0) return()
  tryCatch({
    # If another shiny.cov-instrumented process is writing to the same
    # app_dir concurrently (e.g. two AppDriver sessions against one
    # app_dir), a blind overwrite here would race with its periodic save
    # and silently drop whichever side didn't write last. Merge with
    # whatever's currently on disk -- same srcref-derived key covr itself
    # uses -- taking the max value per key (coverage counts only grow) and
    # the union of keys otherwise. This narrows but doesn't eliminate the
    # race (a third write landing mid-cycle could still be missed) --
    # cheaper than real cross-process locking.
    if (file.exists(output_file)) {
      on_disk <- tryCatch(readRDS(output_file), error = function(e) NULL)
      if (!is.null(on_disk) && length(on_disk) > 0) {
        for (k in names(on_disk)) {
          disk_entry <- on_disk[[k]]
          disk_value <- if (is.list(disk_entry)) disk_entry$value %||% 0L else disk_entry
          mine <- counters[[k]]
          if (is.null(mine)) {
            counters[[k]] <- disk_entry
          } else {
            mine_value <- if (is.list(mine)) mine$value %||% 0L else mine
            if (is.list(mine)) {
              mine$value <- max(mine_value, disk_value)
            } else {
              mine <- max(mine_value, disk_value)
            }
            counters[[k]] <- mine
          }
        }
      }
    }

    # Write atomically: saveRDS() directly to output_file leaves a window
    # where a concurrent process kill (the periodic background save below
    # runs repeatedly for the life of the process, so this isn't a rare
    # one-shot write) truncates the file mid-write, and collect() then
    # fails entirely on a corrupted RDS instead of just using slightly
    # stale data. Write to a temp file in the same directory and rename
    # into place -- rename() is atomic on the same filesystem, so readers
    # only ever see a complete previous version or a complete new one.
    tmp <- paste0(output_file, ".tmp", Sys.getpid())
    saveRDS(counters, tmp)
    file.rename(tmp, output_file)
    if (!quiet) message("shiny.cov: coverage saved (", length(counters), " counters)")
  }, error = function(e) {
    message("shiny.cov: save failed: ", conditionMessage(e))
  })

  # Sibling files, same atomic tmp-then-rename pattern, for the fallback
  # source-line attribution and real module boundaries -- see their hooks
  # above for why each exists. Both are optional: a missing
  # file just means neither R-side helper is available (child process
  # never loaded {htmltools}/{shiny}, or the app has no tag ids/modules),
  # and the R-side consumers (merge_ui_coverage(), apply_module_boundaries())
  # degrade gracefully when absent.
  if (exists(".shinycov_id_srcrefs", where = .GlobalEnv, inherits = FALSE)) {
    id_srcrefs <- as.list(.shinycov_id_srcrefs)
    if (length(id_srcrefs) > 0) {
      tryCatch({
        # Same concurrent-process concern as coverage.rds above: merge
        # with whatever's already on disk (union of ids -- there's no
        # "value" to max() here, just presence) rather than overwriting.
        if (file.exists(id_srcrefs_file)) {
          on_disk <- tryCatch(readRDS(id_srcrefs_file), error = function(e) NULL)
          if (!is.null(on_disk) && length(on_disk) > 0) {
            for (k in names(on_disk)) {
              if (is.null(id_srcrefs[[k]])) id_srcrefs[[k]] <- on_disk[[k]]
            }
          }
        }
        tmp2 <- paste0(id_srcrefs_file, ".tmp", Sys.getpid())
        saveRDS(id_srcrefs, tmp2)
        file.rename(tmp2, id_srcrefs_file)
      }, error = function(e) {
        message("shiny.cov: id_srcrefs save failed: ", conditionMessage(e))
      })
    }
  }
  if (exists(".shinycov_modules", where = .GlobalEnv, inherits = FALSE)) {
    modules <- ls(.shinycov_modules, all.names = TRUE)
    if (length(modules) > 0) {
      tryCatch({
        if (file.exists(modules_file)) {
          on_disk <- tryCatch(readRDS(modules_file), error = function(e) character())
          modules <- union(modules, on_disk)
        }
        tmp3 <- paste0(modules_file, ".tmp", Sys.getpid())
        saveRDS(modules, tmp3)
        file.rename(tmp3, modules_file)
      }, error = function(e) {
        message("shiny.cov: modules save failed: ", conditionMessage(e))
      })
    }
  }
}

# Save after bootstrap loads (definitions)
save_coverage()

# Register shutdown hooks for reactive code executed during app run.
# Quiet: onStop() fires per Shiny *session* ending, not per process exit --
# a long interactive session with several connect/disconnect cycles would
# otherwise print "coverage saved" once per cycle. The truly-final save at
# actual process exit (.Last()/reg.finalizer() below) still messages.
if ("shiny" %in% loadedNamespaces()) {
  tryCatch({
    shiny::onStop(function() save_coverage(quiet = TRUE), session = NULL)
  }, error = function(e) NULL)
}

# Graceful-shutdown hooks (onStop/.Last/reg.finalizer, below) are a
# best-effort mechanism: they rely on the test framework sending an
# interrupt/SIGTERM the R process actually handles before giving up and
# killing it (this is precisely the failure mode behind covr#438 and
# shinytest2#250 -- a 20s graceful stop() with R_COVR=true does not
# reliably trigger any of these hooks on every platform/configuration).
# Don't depend on shutdown working: periodically flush coverage in the
# background so an abrupt kill loses at most a couple of seconds of the
# most recent interaction instead of the entire session's reactive
# coverage. `later` is a hard dependency of httpuv/shiny, so this runs on
# the same event loop Shiny itself uses and fires between request handling
# without needing a session (registration happens before any session
# connects).
if (requireNamespace("later", quietly = TRUE)) {
  .shinycov_last_periodic_save <- Sys.time()
  .shinycov_periodic_save_fast_fires <- 0L
  .shinycov_periodic_save <- function() {
    # `later::later(fn, delay = 2)` only promises a *minimum* delay --
    # something draining the later queue in a tight loop (observed with a
    # rhino app's autoreload file-watcher) can fire this callback
    # ~30x/second instead of every 2s, which both
    # wastes CPU on redundant saves and, since each firing reschedules
    # itself, risks an R call-stack overflow ("evaluation nested too
    # deeply") if the draining happens without unwinding between calls.
    # Enforce real spacing with a wall-clock check, and give up on
    # rescheduling entirely (falling back to the onStop/.Last/
    # reg.finalizer hooks below) if this keeps firing far too fast --
    # better to lose the periodic-save safety net in that rare case than
    # spin or crash.
    now <- Sys.time()
    if (as.numeric(now - .shinycov_last_periodic_save, units = "secs") < 1.5) {
      .shinycov_periodic_save_fast_fires <<- .shinycov_periodic_save_fast_fires + 1L
      if (.shinycov_periodic_save_fast_fires > 20L) {
        message("shiny.cov: periodic background save disabled -- the event ",
                "loop is running abnormally fast in this process. Coverage ",
                "will still be saved via shiny::onStop()/.Last() at shutdown.")
        return(invisible())
      }
      later::later(.shinycov_periodic_save, delay = 2)
      return(invisible())
    }
    .shinycov_periodic_save_fast_fires <<- 0L
    .shinycov_last_periodic_save <<- now
    save_coverage(quiet = TRUE) # runs every ~2s for the life of the
    # process -- see save_coverage()'s `quiet` doc for why this fires
    # silently instead of flooding the console.
    later::later(.shinycov_periodic_save, delay = 2)
  }
  later::later(.shinycov_periodic_save, delay = 2)
}

# `orig_last` must be resolved *eagerly* here in local()'s body, not
# captured as a lazily-evaluated function-argument promise. R doesn't
# force such a promise until the returned closure's body references it,
# which only happens at real R exit -- by which point the `.Last <-`
# assignment below has already completed, so the lexical lookup would
# resolve to this very wrapper, and `orig_last(...)` would recurse into
# itself with no base case until R hits its nesting limit. local()'s body
# runs immediately, so `orig_last` is bound to the real, pre-existing
# `.Last` (or NULL) before the new one is ever assigned.
.Last <- local({
  orig_last <- if (exists(".Last", mode = "function")) .Last else NULL
  function(...) {
    if (!exists(".shinycov_saved", inherits = FALSE)) {
      assign(".shinycov_saved", TRUE, envir = .GlobalEnv)
      save_coverage()
    }
    if (is.function(orig_last)) orig_last(...)
  }
})

reg.finalizer(.GlobalEnv, function(e) {
  # Mirrors the .Last() wrapper's guard above: without it, this finalizer
  # and .Last() both fire on a normal exit, doubling the "coverage saved"
  # message and doing a redundant disk read/write (harmless since
  # save_coverage()'s max()-based merge is idempotent, but confusing to
  # debug).
  if (!exists(".shinycov_saved", envir = e, inherits = FALSE)) {
    assign(".shinycov_saved", TRUE, envir = e)
    save_coverage()
  }
}, onexit = TRUE)

message("shiny.cov: instrumentation active. Output: ", output_file)

}
}
