context("internal")

test_that("internal", {
  e <- list2env(list(a = 1), parent = emptyenv())
  expect_error(try_list(expression(symbol1, symbol2), envir = e))
  expect_equal(try_list(expression(symbol1, symbol2), 0, envir = e), 0)
}) 
