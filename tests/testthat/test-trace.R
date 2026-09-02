# Regression tests for R/trace.R's shinycov_get_parse_data() caching:
# without it, the whole source file gets reparsed from scratch, uncached,
# once per un-braced if/for/while/switch branch encountered during
# instrumentation. It caches the parsed result directly on the
# srcfile object itself (a mutable environment shared by every srcref
# belonging to one parse() call -- see the comment above
# shinycov_get_parse_data() for why that's a safe, correctly-scoped cache
# key with no separate lifecycle bookkeeping). These tests are about
# *correctness* of that cache, not performance: proving branch-level
# tracing still produces the right per-branch counts, and that the cache
# can't bleed stale data across separate files or separate
# instrument_file()/instrument_code() calls.

test_that("shinycov_get_parse_data() caches on the srcfile object and stays correct", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines("if (TRUE) 1 else 2", tmp)
  parsed <- parse(file = tmp, keep.source = TRUE)
  srcfile <- attr(attr(parsed, "srcref")[[1]], "srcfile")

  expect_null(srcfile$.shinycov_parse_data)
  pd1 <- shinycov_get_parse_data(srcfile)
  # Cached onto the srcfile itself after the first call...
  expect_false(is.null(srcfile$.shinycov_parse_data))
  pd2 <- shinycov_get_parse_data(srcfile)
  # ...and the second call returns the identical cached object rather than
  # reparsing.
  expect_identical(pd1, pd2)

  # A different srcfile (its own fresh parse() call, different content)
  # must never see the first one's cached data.
  tmp2 <- tempfile(fileext = ".R")
  on.exit(unlink(tmp2), add = TRUE)
  writeLines("if (FALSE) 3 else 4", tmp2)
  parsed2 <- parse(file = tmp2, keep.source = TRUE)
  srcfile2 <- attr(attr(parsed2, "srcref")[[1]], "srcfile")
  expect_null(srcfile2$.shinycov_parse_data)
  pd3 <- shinycov_get_parse_data(srcfile2)
  expect_false(identical(sort(pd1$text), sort(pd3$text)))
})

test_that("caching does not corrupt branch-level tracing within one file (multiple un-braced ifs)", {
  # Two separate un-braced if/else statements (branches with no {} of their
  # own -- the {} here is only around the *function body*, so each if/else
  # is itself a real block-member statement with its own srcref, and its
  # then/else branches only get imputed per-branch srcrefs via
  # shinycov_impute_srcref() -- the exact path that used to reparse from
  # scratch every time) means shinycov_get_parse_data() gets called more
  # than once against the *same* srcfile -- exactly the repeated-call
  # pattern the cache targets. If the cache returned stale or
  # cross-contaminated parse data, branch attribution here would be wrong.
  code <- c(
    "f1 <- function(x) {",
    "  if (x > 0) \"pos\" else \"neg\"",
    "}",
    "f2 <- function(x) {",
    "  if (x > 10) \"big\" else \"small\"",
    "}"
  )
  inst <- instrument_code(code)
  env <- new.env()
  eval_instrumented(inst, env)

  # f1()/f2() count() calls embedded in their (now-defined) bodies only
  # fire into `inst$counters` while it's the *active* counters environment
  # -- eval_instrumented() already restored the previous one by the time
  # it returns (see its own docs: each file gets an isolated counter env
  # only for the duration of the swap), so calling the functions has to
  # happen with `inst$counters` swapped back in, the same way bootstrap.R
  # keeps a permanent active environment for the life of a child process.
  old_counters <- shinycov_set_counters(inst$counters)
  r1 <- env$f1(5)
  r2 <- env$f2(1)
  shinycov_set_counters(old_counters)

  expect_equal(r1, "pos")
  expect_equal(r2, "small")
  # "neg" and "big" branches deliberately never exercised.

  # Each then/else branch is a one-token literal, imputed as its own
  # srcref distinct from the enclosing if-statement's -- so an *exact*
  # text match (not a substring grepl(), which would also match the whole
  # if/else statement's own text and conflate the two branches) correctly
  # isolates each branch's own counter.
  counter_text <- function(entry) {
    if (is.null(entry$srcref)) return("")
    paste(as.character(entry$srcref), collapse = "\n")
  }
  value_for <- function(text) {
    entries <- as.list(inst$counters)
    hits <- Filter(function(e) identical(counter_text(e), text), entries)
    expect_gt(length(hits), 0) # sanity: the branch really did get its own counter
    sum(vapply(hits, function(e) e$value %||% 0L, integer(1)))
  }

  expect_equal(value_for("\"pos\""), 1L)
  expect_equal(value_for("\"neg\""), 0L)
  expect_equal(value_for("\"small\""), 1L)
  expect_equal(value_for("\"big\""), 0L)
})

test_that("caching does not leak across separate instrument_code() calls on different files", {
  # Both files share the same shape and the same line number for their
  # un-braced if/else -- if the cache were keyed by anything other than
  # the (always-fresh-per-parse) srcfile object itself, this is exactly
  # the shape of bug that would surface as one file's branch text leaking
  # into the other's counters.
  code_a <- "g <- function(x) if (x > 0) \"A-pos\" else \"A-neg\""
  code_b <- "g <- function(x) if (x > 0) \"B-pos\" else \"B-neg\""

  inst_a <- instrument_code(code_a)
  inst_b <- instrument_code(code_b)

  env_a <- new.env()
  env_b <- new.env()
  eval_instrumented(inst_a, env_a)
  eval_instrumented(inst_b, env_b)

  expect_equal(env_a$g(1), "A-pos")
  expect_equal(env_b$g(-1), "B-neg")

  texts_in <- function(counter_env) {
    vapply(as.list(counter_env), function(e) {
      if (is.null(e$srcref)) "" else paste(as.character(e$srcref), collapse = "\n")
    }, character(1))
  }
  a_texts <- texts_in(inst_a$counters)
  b_texts <- texts_in(inst_b$counters)

  expect_true(any(grepl("A-pos", a_texts, fixed = TRUE)))
  expect_false(any(grepl("B-", a_texts, fixed = TRUE)))
  expect_true(any(grepl("B-neg", b_texts, fixed = TRUE)))
  expect_false(any(grepl("A-", b_texts, fixed = TRUE)))
})
