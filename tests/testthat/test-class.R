context("class")

test_that("list.class", {

  # simple list
  x <- list(a = 1, b = 2, c = 3, d = 2, e = 3, f = 1)
  expect_identical(list.class(x, .), list(`1` = list(a = 1, f = 1), `2` = list(b = 2,
    d = 2), `3` = list(c = 3, e = 3)))

  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_identical(list.class(x, unlist(score)), list(`7` = x["p3"], `8` = x["p1"],
    `9` = x[c("p2", "p3")], `10` = x["p1"]))
  lapply(8:10, function(i) list.class(x, score$c1 > i))
})
