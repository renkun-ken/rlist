context("find")

test_that("list.findi", {
  x <- list(1, 2, c(3, 4))
  expect_identical(list.findi(x, any(. >= 2)), 2L)
  expect_identical(list.findi(x, any(. >= 2), 2), c(2L, 3L))
  expect_identical(list.findi(x, any(. >= 2), 3), c(2L, 3L))
  expect_identical(list.findi(x, any(. >= 2), 4), c(2L, 3L))
})

test_that("list.find", {

  # simple list
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))

  expect_identical(list.find(x, type == "B", 1), x[2])
  expect_identical(list.find(x, type == "B", 2), x[c(2, 3)])

  # list of vectors
  x <- list(a = c(x = 1, y = 2), b = c(x = 3, y = 4))
  expect_identical(list.find(x, sum(.) >= 4), x[2])

  # list of lists
  l1 <- list(a = list(x = 1, y = 2), b = list(x = 2, y = 3))
  expect_identical(list.find(l1, sum(unlist(.)) <= 4), l1[1])

  lapply(2:4, function(i) list.find(l1, sum(unlist(.)) <= i))
})
