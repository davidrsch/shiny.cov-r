# AST tracing/counting engine -- ported from {covr} (MIT licensed,
# Copyright (c) covr authors: https://github.com/r-lib/covr), not called
# via covr::: at runtime.
#
# Why: shiny.cov previously called covr:::trace_calls()/covr:::count()/
# covr:::.counters directly (unexported internals, accessed via `:::`,
# reached by unlockBinding()/lockBinding() gymnastics on covr's own
# namespace at runtime). That worked, but had two real costs: an
# unavoidable R CMD check NOTE ("Unexported object imported by a ':::'
# call"), and a standing fragility risk -- covr doesn't promise these
# internals stay stable across versions, since they were never part of
# its public API. Both packages are MIT licensed, so porting this code
# in-tree is permitted; owning it directly also means the counter-swap
# mechanism below no longer needs any namespace-locking tricks at all
# (see shinycov_set_counters()), since shiny.cov is now mutating its own
# state, not reaching into someone else's namespace.
#
# trace_calls() itself (the core AST-walking algorithm) is ported
# essentially verbatim; the one deliberate behavioral simplification is
# in shinycov_get_parse_data() -- see the comment there for why.

# ---- Active counters state ----
#
# A single mutable environment, never reassigned (so it's never subject to
# R's namespace-binding-locking -- only its *contents* change, via normal
# environment mutation, which locking a binding doesn't restrict). Holds
# the currently-"active" counters environment (swappable, mirroring
# covr:::.counters' own swap-based design -- see shinycov_set_counters())
# and the srcref most recently counted (used by the htmltools::tag()/
# tagAppendAttributes() source-attribution hooks in bootstrap.R, which
# read it via shinycov_last_srcref() -- no runtime patching of this
# function needed anymore, since shiny.cov owns it outright now).
.shinycov_state <- new.env(parent = emptyenv())
.shinycov_state$counters <- new.env(parent = emptyenv())
.shinycov_state$last_srcref <- NULL

#' Get the currently-active counters environment
#' @return An environment.
#' @keywords internal
shinycov_get_counters <- function() {
  .shinycov_state$counters
}

#' Swap in a new active counters environment
#'
#' Every `count()` call embedded in instrumented code writes into whatever
#' environment is currently active here -- swapping it lets
#' `instrument_file()` give each file its own isolated counters during
#' instrumentation (needed so the package's own tests, which call
#' `instrument_file()` many times in one R session, don't see each other's
#' entries), and lets `bootstrap.R` swap in one permanent, process-wide
#' environment for the life of a child Shiny process.
#'
#' @param env The new active counters environment.
#' @return The previous active counters environment, invisibly (so callers
#'   can restore it later).
#' @keywords internal
shinycov_set_counters <- function(env) {
  old <- .shinycov_state$counters
  .shinycov_state$counters <- env
  invisible(old)
}

#' The srcref most recently passed to `shinycov_count()`
#'
#' Used by `bootstrap.R`'s `htmltools::tag()`/`tagAppendAttributes()` hooks
#' to attribute a UI element's id to the source line that was executing
#' when it was constructed -- see the comments there for the full
#' rationale for attributing computed/dynamic ids to a source line.
#'
#' @return A `srcref` object, or `NULL` if nothing has been counted yet.
#' @keywords internal
shinycov_last_srcref <- function() {
  .shinycov_state$last_srcref
}

# ---- Ported from covr (see file header) ----

#' @keywords internal
shinycov_source_filename <- function(x) {
  res <- utils::getSrcFilename(x, full.names = FALSE, unique = TRUE)
  if (length(res) == 0) {
    return("")
  }
  res
}

#' @keywords internal
shinycov_key <- function(x) {
  paste(collapse = ":", c(shinycov_source_filename(x), x))
}

#' @keywords internal
shinycov_count <- function(key) {
  counters <- .shinycov_state$counters
  entry <- counters[[key]]
  entry$value <- (entry$value %||% 0L) + 1L
  counters[[key]] <- entry
  .shinycov_state$last_srcref <- entry$srcref
}

#' @keywords internal
shinycov_new_counter <- function(src_ref, parent_functions) {
  k <- shinycov_key(src_ref)
  counters <- .shinycov_state$counters
  counters[[k]] <- list(value = 0L, srcref = src_ref, functions = parent_functions)
  k
}

#' @keywords internal
shinycov_is_na <- function(x) {
  !is.null(x) && !is.symbol(x) && is.na(x)
}

#' @keywords internal
shinycov_is_brace <- function(x) {
  is.symbol(x) && as.character(x) == "{"
}

#' @keywords internal
shinycov_is_conditional_or_loop <- function(x) {
  is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("if", "for", "else", "switch")
}

#' Parse data for a srcfile, for `shinycov_impute_srcref()`'s use
#'
#' covr's own `get_parse_data()` additionally supports `#line` directives
#' (via the `rex` package plus a small file-content cache) -- a feature
#' for preprocessed/generated source files (e.g. Rcpp attributes output)
#' that remap positions back to an original, different file. shiny.cov
#' only ever instruments plain `.R` files it parses itself, which never
#' contain `#line` directives, so that whole layer is dead code here --
#' dropped rather than carried over, to avoid a new dependency (`rex`)
#' for a code path that can't be reached from this package's own usage.
#'
#' Cached directly on `srcfile` itself: every top-level srcref belonging to
#' one parsed file shares a single, mutable srcfile environment (already
#' relied on elsewhere -- see `box_hook.R`'s own mutation of `sf$filename`
#' for the same object), and every `parse()` call allocates a brand-new
#' srcfile object, even for the same file path parsed twice. So caching
#' here is scoped exactly right with no separate cache-lifecycle/key
#' bookkeeping needed: reused for every `shinycov_impute_srcref()` call
#' made while tracing this one parse (can otherwise mean re-parsing the
#' whole file from scratch once per un-braced `if`/`for`/`while`/`switch`
#' branch encountered), and it can never go stale across a different
#' `instrument_file()` call or a file's contents changing, since those
#' always get their own fresh srcfile object from a fresh `parse()`.
#'
#' @keywords internal
shinycov_get_parse_data <- function(srcfile) {
  cached <- srcfile$.shinycov_parse_data
  if (!is.null(cached)) {
    return(cached)
  }
  lines <- getSrcLines(srcfile, 1L, Inf)
  parse_data <- utils::getParseData(parse(text = lines, keep.source = TRUE), includeText = TRUE)
  srcfile$.shinycov_parse_data <- parse_data
  parse_data
}

#' @keywords internal
shinycov_get_tokens <- function(srcref) {
  shinycov_get_parse_data(attr(srcref, "srcfile"))
}

#' Synthesize a srcref for a conditional/loop branch that doesn't have one
#'
#' `if`/`for`/`while`/`switch` written without braces around their
#' branches (a common, valid R style) don't get their own srcref from the
#' parser the way a brace-delimited block's members do -- this reconstructs one from
#' token-level parse data so `shinycov_trace_calls()` can still
#' instrument each branch individually instead of collapsing the whole
#' statement into one counter.
#'
#' @keywords internal
shinycov_impute_srcref <- function(x, parent_ref) {
  if (!shinycov_is_conditional_or_loop(x)) {
    return(NULL)
  }
  if (is.null(parent_ref)) {
    return(NULL)
  }
  pd <- shinycov_get_tokens(parent_ref)
  pd_expr <- ((pd$line1 == parent_ref[[1L]] & pd$line2 == parent_ref[[3L]]) |
    (pd$line1 == parent_ref[[7L]] & pd$line2 == parent_ref[[8L]])) &
    pd$col1 == parent_ref[[2L]] & pd$col2 == parent_ref[[4L]] &
    pd$token == "expr"
  pd_expr_idx <- which(pd_expr)
  if (length(pd_expr_idx) == 0L) {
    return(NULL)
  }
  if (length(pd_expr_idx) > 1) {
    pd_expr_idx <- pd_expr_idx[[1]]
  }
  expr_id <- pd$id[pd_expr_idx]
  pd_child <- pd[pd$parent == expr_id, ]
  pd_child <- pd_child[order(pd_child$line1, pd_child$col1), ]
  pd_child <- pd_child[pd_child$token != "COMMENT", ]
  if (pd$line1[pd_expr_idx] == parent_ref[[7L]] & pd$line2[pd_expr_idx] == parent_ref[[8L]]) {
    line_offset <- parent_ref[[7L]] - parent_ref[[1L]]
  } else {
    line_offset <- 0
  }
  make_srcref <- function(from, to = from) {
    if (length(from) == 0) {
      return(NULL)
    }
    srcref(
      attr(parent_ref, "srcfile"),
      c(
        pd_child$line1[from] - line_offset, pd_child$col1[from],
        pd_child$line2[to] - line_offset, pd_child$col2[to],
        pd_child$col1[from], pd_child$col2[to],
        pd_child$line1[from], pd_child$line2[to]
      )
    )
  }
  switch(as.character(x[[1L]]),
    `if` = {
      src_ref <- list(NULL, make_srcref(3), make_srcref(5), make_srcref(7))
      src_ref[seq_along(x)]
    },
    `for` = {
      list(NULL, NULL, make_srcref(2), make_srcref(3))
    },
    `while` = {
      list(NULL, make_srcref(3), make_srcref(5))
    },
    switch = {
      exprs <- utils::tail(which(pd_child$token == "expr"), n = -1)
      token <- pd_child$token
      next_token <- c(utils::tail(token, n = -1), NA_character_)
      drops <- which(token == "EQ_SUB" & next_token != "expr")
      exprs <- sort(c(exprs, drops))
      ignore_drop_through <- function(x) {
        if (x %in% drops) {
          return(NULL)
        }
        x
      }
      exprs <- lapply(exprs, ignore_drop_through)
      ignore_dots <- function(x) {
        if (identical("...", pd$text[pd$parent == pd_child$id[x]])) {
          return(NULL)
        }
        x
      }
      exprs <- lapply(exprs, ignore_dots)
      c(list(NULL), lapply(exprs, make_srcref))
    },
    NULL
  )
}

#' Recursively instrument an AST with counter-increment calls
#'
#' Walks `x` and wraps every branch/statement that has (or can be given) a
#' srcref with a call to `shinycov_count()`, so evaluating the result
#' records exactly which branches actually ran -- the same real,
#' branch-level tracking `covr::package_coverage()` gives a package, not
#' just "did this expression get reached at all."
#'
#' @param x A language object, function, or list of either.
#' @param parent_functions Character vector of enclosing function names,
#'   for attribution in the resulting coverage entries.
#' @param parent_ref The nearest enclosing srcref, used to impute one for
#'   children that don't have their own (see `shinycov_impute_srcref()`).
#' @return `x`, with counter-increment calls woven in.
#' @keywords internal
shinycov_trace_calls <- function(x, parent_functions = NULL, parent_ref = NULL) {
  count <- function(key, val) {
    call("if", TRUE, call("{", as.call(list(
      call(":::", as.symbol("shiny.cov"), as.symbol("shinycov_count")), key
    )), val))
  }

  if (is.null(parent_functions)) {
    parent_functions <- deparse(substitute(x))
  }
  recurse <- function(y) {
    lapply(y, shinycov_trace_calls, parent_functions = parent_functions)
  }

  if (is.atomic(x) || is.name(x) || is.null(x)) {
    if (is.null(parent_ref)) {
      x
    } else {
      if (shinycov_is_na(x) || shinycov_is_brace(x)) {
        x
      } else {
        key <- shinycov_new_counter(parent_ref, parent_functions)
        count(key, x)
      }
    }
  } else if (is.call(x)) {
    src_ref <- attr(x, "srcref") %||% shinycov_impute_srcref(x, parent_ref)
    if ((identical(x[[1]], as.name("<-")) || identical(x[[1]], as.name("="))) &&
      (is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function")))) {
      parent_functions <- c(parent_functions, as.character(x[[2]]))
    }
    if (identical(x[[1]], as.name("{")) && length(x) == 2 &&
      is.call(x[[2]]) && identical(x[[2]][[1]], as.name("{"))) {
      as.call(x)
    } else if (!is.null(src_ref)) {
      as.call(Map(shinycov_trace_calls, x, src_ref, MoreArgs = list(parent_functions = parent_functions)))
    } else if (!is.null(parent_ref)) {
      key <- shinycov_new_counter(parent_ref, parent_functions)
      count(key, as.call(recurse(x)))
    } else {
      as.call(recurse(x))
    }
  } else if (is.function(x)) {
    if (is.primitive(x)) {
      return(x)
    }
    fun_body <- body(x)
    if (!is.null(attr(x, "srcref")) && (is.symbol(fun_body) || !identical(fun_body[[1]], as.name("{")))) {
      src_ref <- attr(x, "srcref")
      key <- shinycov_new_counter(src_ref, parent_functions)
      fun_body <- count(key, shinycov_trace_calls(fun_body, parent_functions))
    } else {
      fun_body <- shinycov_trace_calls(fun_body, parent_functions)
    }
    new_formals <- shinycov_trace_calls(formals(x), parent_functions)
    if (is.null(new_formals)) {
      new_formals <- list()
    }
    formals(x) <- new_formals
    body(x) <- fun_body
    x
  } else if (is.pairlist(x)) {
    as.pairlist(recurse(x))
  } else if (is.expression(x)) {
    as.expression(recurse(x))
  } else if (is.list(x)) {
    recurse(x)
  } else {
    message("Unknown language class: ", paste(class(x), collapse = "/"))
    x
  }
}
