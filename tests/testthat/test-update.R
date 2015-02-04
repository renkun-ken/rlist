context("update")
test_that("list.update", {
  # simple list
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_identical(list.update(x, type = NULL), lapply(x, function(xi) {
    xi[-1]
  }))
  expect_identical(list.update(x, score = list(min = min(unlist(score)))), lapply(x,
    function(xi) {
      modifyList(xi, list(score = list(min = min(unlist(xi$score)))))
    }))
  expect_identical(list.update(x, range = range(unlist(score))), lapply(x, function(xi) {
    modifyList(xi, list(range = range(unlist(xi$score))))
  }))
  expect_identical(list.update(x, n = length(.)), lapply(x, function(xi) {
    modifyList(xi, list(n = length(xi)))
  }))
  lapply(1:3, function(i) list.update(x, c = i))
})
