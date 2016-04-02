context("group")

test_that("list.group", {

  # simple list
  x <- list(a = 1, b = 2, c = 3, d = 2, e = 3, f = 1)
  expect_identical(list.group(x, .), list(`1` = list(a = 1, f = 1), `2` = list(b = 2,
    d = 2), `3` = list(c = 3, e = 3)))

  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_identical(list.group(x, type), list(A = x["p1"], B = x[c("p2", "p3")]))
  expect_identical(list.group(x, mean(unlist(score))), list(`8` = x["p3"], `9` = x[c("p1",
    "p2")]))

  expect_identical(list.group(1:10, . %% 3, . %% 2), structure(list(`0` = structure(list(`0` = 6L,
    `1` = c(3L, 9L)), .Names = c("0", "1")), `1` = structure(list(`0` = c(4L,
    10L), `1` = c(1L, 7L)), .Names = c("0", "1")), `2` = structure(list(`0` = c(2L,
    8L), `1` = 5L), .Names = c("0", "1"))), .Names = c("0", "1", "2")))

  expect_identical(list.group(c(3, 1, 3, 3, 2, 2), letters[.], sorted = FALSE),
    list(c = c(3, 3, 3), a = 1, b = c(2, 2)))
  expect_identical(list.group(c(3, 1, 3, 3, 2, 2), letters[.], sorted = TRUE),
    list(a = 1, b = c(2, 2), c = c(3, 3, 3)))

  # test dynamic scoping
  lapply(2:4, function(i) list.group(x, sum(unlist(score)) <= i))
})

test_that("list.ungroup", {
  xg <- list(`1` = list(a = 1, f = 1), `2` = list(b = 2, d = 2), `3` = list(c = 3,
    e = 3))
  xg2 <- list(a = list(a = 1, f = 1), c = list(b = 2, d = 2), b = list(c = 3, e = 3))
  expect_identical(list.ungroup(xg, sort.names = TRUE), list(a = 1, b = 2, c = 3,
    d = 2, e = 3, f = 1))
  expect_identical(list.ungroup(xg2, sort.names = FALSE), list(a = 1, f = 1, b = 2,
    d = 2, c = 3, e = 3))

  x <- list(a = list(a1 = list(x=list(x1=2,x2=3),y=list(y1=1,y2=3)), a0 = list(x=list(x1=1,x2=5),y=list(y1=0,y2=1))),
    b = list(b1 = list(x=list(x1=2,x2=6),y=list(y1=3,y2=2))))
  expect_identical(list.ungroup(x, level = 1L), c(x$a, x$b))
  expect_identical(list.ungroup(x, level = 2L), c(unlist(unname(x$a), recursive = FALSE), unlist(unname(x$b), recursive = FALSE)))
  expect_identical(list.ungroup(x, level = 1L, group.names = TRUE), unlist(x, recursive = FALSE))
  expect_identical(list.ungroup(x, level = 2L, group.names = TRUE), c(unlist(unlist(x, recursive = FALSE), recursive = FALSE)))
})
