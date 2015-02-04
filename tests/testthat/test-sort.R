context("sort")

test_that("list.sort", {

  # simple list
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))

  expect_identical(list.sort(x, type, (score$c2)), x[c(1, 2, 3)])
  expect_identical(list.sort(x, min(score$c1, score$c2)), x[c(3, 1, 2)])

  # list of vectors
  x <- list(a = c(x = 1, y = 2), b = c(x = 3, y = 4))
  expect_identical(list.sort(x, sum(.)), x[c(1, 2)])
  expect_identical(list.sort(x, (sum(.))), x[c(2, 1)])

  expect_identical(list.sort(c("a", "b", "c")), c("a", "b", "c"))
  expect_identical(list.sort(c("a", "b", "c"), (.)), c("c", "b", "a"))
  expect_identical(list.sort(list("a", "b", "c"), .), list("a", "b", "c"))
  expect_identical(list.sort(list("a", "b", "c"), (.)), list("c", "b", "a"))

  # warning in irregular cases
  expect_warning(list.sort(list(1, 2, c(2, 3)), .), "^Non-single value in column")
  expect_warning(list.sort(list(1, 2, "a"), .), "^Inconsistent classes")

  lapply(1:3, function(i) list.sort(x, sum(.) + i))
})

test_that("list.order", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))

  expect_equal(list.order(x, type, (score$c2)), c(1, 2, 3))
  expect_equal(list.order(x, min(score$c1, score$c2)), c(3, 1, 2))

  x <- list(a = c(x = 1, y = 2), b = c(x = 3, y = 4))
  expect_equal(lapply(1:3, function(i) list.order(x, sum(.) + i)), list(c(1, 2),
    c(1, 2), c(1, 2)))

  # warning in irregular cases
  expect_warning(list.order(list(1, 2, c(2, 3)), .), "^Non-single value in column")
  expect_warning(list.order(list(1, 2, "a"), .), "^Inconsistent classes")
})
