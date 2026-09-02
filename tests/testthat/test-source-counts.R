test_that("sum_counters() sums per key across sources", {
  mk <- function(v1, v2) list(
    k1 = list(value = v1, srcref = NULL, functions = character(0)),
    k2 = list(value = v2, srcref = NULL, functions = character(0))
  )
  s <- sum_counters(list(cypress = mk(3, 1), shinytest2 = mk(2, 5)))
  expect_equal(s$k1$value, 5)
  expect_equal(s$k2$value, 6)
})

test_that("shinycov_source_from_file() maps filenames to sources", {
  srcs <- shinycov_source_from_file(c(
    "coverage.rds", "coverage.cypress.rds", "coverage.playwright.rds"
  ))
  expect_equal(unname(srcs), c("total", "cypress", "playwright"))
})

test_that("source_counts() summarises per-source hits", {
  cov <- list(
    a = list(srcref = NULL, value = 5, functions = character(0))
  )
  class(cov) <- c("coverage", "list")
  attr(cov, "shinycov_sources") <- list(
    cypress = list(k1 = list(value = 3, srcref = NULL, functions = character(0))),
    shinytest2 = list(k1 = list(value = 2, srcref = NULL, functions = character(0)))
  )
  df <- source_counts(cov)
  expect_equal(df$source, c("cypress", "shinytest2"))
  expect_equal(df$hits, c(3, 2))
})
