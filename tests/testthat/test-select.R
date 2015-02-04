context("select")
test_that("list.select", {
  # simple list
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_identical(list.select(x, type), lapply(x, function(xi) {
    xi["type"]
  }))
  expect_identical(list.select(x, type, score), lapply(x, function(xi) {
    xi[c("type", "score")]
  }))
  expect_identical(list.select(x, range = range(unlist(score))), lapply(x, function(xi) {
    list(range = range(unlist(xi$score)))
  }))
  expect_identical(list.select(x, n = length(.)), lapply(x, function(xi) {
    list(n = length(xi))
  }))
  lapply(1:3, function(i) list.select(x, p = score$c1 + i))
})
