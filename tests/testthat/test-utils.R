context("utils")

test_that("tryGet", {
  e <- new.env()
  e$x <- 1
  expect_identical(tryGet(x, 0, envir = e), e$x)
  expect_identical(with(e, tryGet(x, 0)), e$x)
  expect_identical(tryGet(y, 0), 0)
  expect_error(tryGet(y + 1, 0))
  expect_identical(list.map(list(list(a = 1, b = 2), list(a = 1, c = 2)), a + tryGet(b, 
    0) + tryGet(c, 0)), list(3, 3))
})

test_that("tryEval", {
  e <- new.env()
  e$x <- 1
  expect_identical(with(e, tryEval(x, 0)), e$x)
  expect_identical(tryEval(y, 0), 0)
  expect_identical(list.map(list(list(a = 1, b = 2), list(a = 1, c = 2)), tryEval(a + 
    b + c, 0)), list(0, 0))
}) 
