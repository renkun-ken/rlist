context("subset")

test_that("subset.list", {

  # simple list
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))

  expect_identical(subset(x, type == "B"), x[c(2, 3)])
  expect_identical(subset(x, type == "B", score$c1), list(p2 = 9, p3 = 9))

  expect_identical(subset(x, type == "B", item ~ item$score$c2), list(p2 = 9, p3 = 7))

  # scoping
  lapply(1:3, function(i) subset(x, score$c2 >= 7 + i, score$c1 + i))

  # list of vectors
  x <- list(a = c(x = 1, y = 2), b = c(x = 3, y = 4))
  expect_identical(subset(x, .["x"] >= 2, .["y"]), list(b = c(y = 4)))
  expect_identical(subset(x, sum(.) <= 4, max(.)), list(a = 2))

  # list of lists

  l1 <- list(a = list(x = 1, y = 2), b = list(x = 2, y = 3))
  expect_identical(subset(l1, sum(unlist(.)) <= 4, unlist(.)), list(a = c(x = 1,
    y = 2)))

  # list of objects of list mode
  l2 <- lapply(1:10, function(i) {
    x <- rnorm(100)
    y <- 2 * x + rnorm(100) * 0.2
    lm(y ~ x)
  })

  expect_identical(subset(l2, mean(residuals) >= 0, mean(residuals^2)), {
    lst <- lapply(l2, function(item) {
      if (mean(item$residuals) >= 0) {
        mean(item$residuals^2)
      }
    })
    lst[vapply(lst, is.null, logical(1))] <- NULL
    lst
  })

  expect_identical(subset(l2, mean(.$residuals) >= 0, mean(.$residuals^2)), {
    lst <- lapply(l2, function(item) {
      if (mean(item$residuals) >= 0) {
        mean(item$residuals^2)
      }
    })
    lst[vapply(lst, is.null, logical(1))] <- NULL
    lst
  })

  expect_identical(subset(l2, mean(resid(.)) >= 0, mean(resid(.)^2)), {
    lst <- lapply(l2, function(item) {
      if (mean(item$residuals) >= 0) {
        mean(item$residuals^2)
      }
    })
    lst[vapply(lst, is.null, logical(1))] <- NULL
    lst
  })

  # list of S4 objects

  # list of other objects


})
