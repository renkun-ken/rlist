context("search")

test_that("list.search", {

  # logical search
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))

  expect_identical(list.search(x, identical(., "A")), list(p1.type = "A"))
  expect_identical(list.search(x, identical(., "A"), unlist = TRUE), c(p1.type = "A"))
  expect_identical(list.search(x, identical(., 9)), list(p2.score.c1 = 9, p2.score.c2 = 9,
    p3.score.c1 = 9))

  x <- list(p1 = list(x = c("A", "B", "C"), y = list(y1 = "A", y2 = c("B", "C"))),
    p2 = list(a = c("A", "B"), b = list(b1 = c("B", "C"), b2 = list("C", "B"))))

  expect_identical(list.search(x, all(. == "A")), list(p1.y.y1 = "A"))

  expect_identical(list.search(x, any("A" %in% .)), list(p1.x = c("A", "B", "C"),
    p1.y.y1 = "A", p2.a = c("A", "B")))

  # fuzzy search
  x <- list(p1 = list(name = "Ken", age = 24), p2 = list(name = "Kent", age = 26),
    p3 = list(name = "Sam", age = 24), p4 = list(name = "Keynes", age = 30),
    p5 = list(name = "Kwen", age = 31))
  expect_equal(list.search(x, grepl("^K\\w+[ts]", .), "character", unlist = TRUE),
    c(p2.name = "Kent", p4.name = "Keynes"))

  y <- list(n = 1:10, list(df = data.frame(id = 1:10, letter = letters[1:10], stringsAsFactors = F)),
    list("aa", "bb"))

  expect_identical(list.search(y, .[grepl("a", .)], "character"), list(df.letter = "a",
    "aa"))

  expect_error(list.search(y, . <= p))
})

test_that("stringdist", {
  skip_if_not_installed("stringdist")

  x <- list(p1 = list(name = "Ken", age = 24), p2 = list(name = "Kent", age = 26),
    p3 = list(name = "Sam", age = 24), p4 = list(name = "Keynes", age = 30),
    p5 = list(name = "Kwen", age = 31))
  
  expect_equal(list.search(x, any(stringdist::stringdist(., "Ken") <= 1), "character",
    unlist = TRUE), c(p1.name = "Ken", p2.name = "Kent", p5.name = "Kwen"))
  expect_identical(list.search(x, all(stringdist::stringdist(., "Ken") == 0), "character"),
    list(p1.name = "Ken"))
  
  x <- list(p1 = list(name = c("Ken", "Ren"), age = 24), p2 = list(name = c("Kent",
    "Potter"), age = 26), p3 = list(name = c("Sam", "Lee"), age = 24), p4 = list(name = c("Keynes",
    "Bond"), age = 30), p5 = list(name = c("Kwen", "Hu"), age = 31))
  expect_equal(list.search(x, all(stringdist::stringdist(., "Ken") <= 1), "character"),
    list(p1.name = c("Ken", "Ren")))
  expect_equal(list.search(x, any(stringdist::stringdist(., "Ken") <= 1), "character"),
    list(p1.name = c("Ken", "Ren"), p2.name = c("Kent", "Potter"), p5.name = c("Kwen",
      "Hu")))
  expect_equal(list.search(x, !any(stringdist::stringdist(., "Ken") <= 1), "character"),
    list(p3.name = c("Sam", "Lee"), p4.name = c("Keynes", "Bond")))
})

test_that("counting", {
  x <- list(p1 = list(name = "Ken", age = 24), p2 = list(name = "Kent", age = 26),
    p3 = list(name = "Sam", age = 24), p4 = list(name = "Keynes", age = 30),
    p5 = list(name = "Kwen", age = 31))
  expect_equal(list.search(x, n ~ n >= 25, "numeric", n = 2, unlist = TRUE), c(p2.age = 26,
    p4.age = 30))
  expect_error(list.search(x, . <= p))
})
