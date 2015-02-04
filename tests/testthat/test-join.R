context("join")
test_that("list.join", {
  l1 <- list(p1 = list(name = "Ken", age = 20), p2 = list(name = "James", age = 21),
    p3 = list(name = "Jenny", age = 20))
  l2 <- list(p1 = list(name = "Jenny", age = 20, type = "A"), p2 = list(name = "Ken",
    age = 20, type = "B"), p3 = list(name = "James", age = 21, type = "A"))
  l3 <- list(p1 = list(name = "Ken", age = 20, type = "B"), p2 = list(name = "James",
    age = 21, type = "A"), p3 = list(name = "Jenny", age = 20, type = "A"))
  expect_identical(list.join(l1, l2, name), l3)
  expect_identical(list.join(l1, l2, .[c("name", "age")]), l3)
  expect_identical(list.join(l1, l2, .[c("name", "age")], .[c("name", "age")]),
    l3)
  expect_error(list.join(l1, l2, name, age))
})
