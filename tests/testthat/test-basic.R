context("basic")

test_that("list.append", {
  # atomic vector
  expect_identical(list.append(c(1, 2, 3), 4, 5), c(1, 2, 3, 4, 5))
  expect_identical(list.append(c(a = 1, b = 2), c = 3), c(a = 1, b = 2, c = 3))
  # simple list
  x <- list(a = 1, b = 2)
  expect_identical(list.append(x, c = 3), c(x, c = 3))
  expect_identical(lapply(1:2, function(i) list.append(x, d = i)), lapply(1:2,
    function(i) c(x, d = i)))

  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  p4 <- list(type = "A", score = list(c1 = 10, ce = 6))
  expect_identical(list.append(x, p4 = p4), c(x, p4 = list(p4)))
})

test_that("list.prepend", {
  # atomic vector
  expect_identical(list.prepend(c(1, 2, 3), 4, 5), c(4, 5, 1, 2, 3))
  expect_identical(list.prepend(c(a = 1, b = 2), c = 3), c(c = 3, a = 1, b = 2))

  # simple list
  x <- list(a = 1, b = 2)
  expect_identical(list.prepend(x, c = 3), c(c = 3, x))
  expect_identical(lapply(1:2, function(i) list.prepend(x, d = i)), lapply(1:2,
    function(i) c(list(d = i), x)))

  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  p0 <- list(type = "A", score = list(c1 = 10, ce = 6))
  expect_identical(list.prepend(x, p0 = p0), c(p0 = list(p0), x))
})

test_that("list.insert", {
  expect_identical(list.insert(c(1, 2, 3), 2, 0), c(1, 0, 2, 3))
  x <- list(a = 1, b = 2, c = 3)
  expect_identical(list.insert(x, 2, q = 0), list(a = 1, q = 0, b = 2, c = 3))
  expect_identical(lapply(1:2, function(i) list.insert(x, 2, q = i)), lapply(1:2,
    function(i) list(a = 1, q = i, b = 2, c = 3)))
})

test_that("list.extract", {

  # simple list
  x <- list(a = 1, b = 2, c = 3)
  expect_identical(list.extract(x, 1), x[[1]])
  expect_identical(list.extract(x, "a"), x[["a"]])
  expect_identical(lapply(1:2, function(i) list.extract(x, i)), lapply(1:2, function(i) x[[i]]))
})

test_that("list.subset", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_identical(list.subset(x, c("p1", "p2")), x[c("p1", "p2")])
  expect_identical(list.subset(x, grepl("^p", names(x))), x[])
})

test_that("list.count", {

  # simple list
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))

  expect_equal(list.count(x, type == "B"), 2)
  expect_equal(list.count(x, score$c1 < 10), 2)
  expect_equal(lapply(c(8, 9, 10), function(i) list.count(x, score$c1 <= i)), list(0,
    2, 3))

  # list of vectors
  x <- list(a = c(x = 1, y = 2), b = c(x = 3, y = 4))
  expect_equal(list.count(x, sum(.) >= 3), 2)
  expect_equal(list.count(x, mean(.) >= 3), 1)

})

test_that("list.reverse", {

  # simple list
  x <- list(a = 1, b = 2, c = 3)
  expect_identical(list.reverse(x), x[c(3, 2, 1)])

  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))

  expect_identical(list.reverse(x), x[c(3, 2, 1)])
})

test_that("list.merge", {

  # simple list
  x <- list(a = 1, b = 2, c = list(x = 1, y = 2))
  expect_identical(list.merge(x, list(b = 5)), list(a = 1, b = 5, c = list(x = 1,
    y = 2)))
  expect_identical(list.merge(x, list(c = list(z = 3))), list(a = 1, b = 2, c = list(x = 1,
    y = 2, z = 3)))

  # multiple lists
  l1 <- list(a = 1, b = list(x = 1, y = 1))
  l2 <- list(a = 2, b = list(z = 2))
  l3 <- list(a = 2, b = list(x = 3))
  expect_identical(list.merge(l1, l2, l3), modifyList(modifyList(l1, l2), l3))

})

test_that("list.do", {
  expect_equal(list.do(list(1, 2, 3), sum), sum(1:3))
  expect_equal(list.do(list(1, 2, 3), "sum"), sum(1:3))
})

test_that("list.apply", {
  expect_identical(list.apply(c(1, 2, 3), "+", 1), list(2, 3, 4))
  expect_identical(list.apply(c(1, 2, 3), `+`, 1), list(2, 3, 4))
})

test_that("list.rbind", {
  x <- lapply(1:10, function(i) c(a = i, b = i^2))
  expect_identical(list.rbind(x), do.call(rbind, x))
})

test_that("list.cbind", {
  x <- list(data.frame(a = rnorm(10), b = rnorm(10)), data.frame(c = rnorm(10),
    d = rnorm(10)), data.frame(e = rnorm(10), f = rnorm(10)))
  expect_identical(list.cbind(x), do.call(cbind, x))
})

test_that("list.stack", {
  x <- lapply(1:10, function(i) list(a = i, b = i^2))
  expect_false(is.null(list.stack(x)))
  x <- lapply(1:10, function(i) list(a = c(i, i + 1), b = c(i^2, i^2 + 1)))
  expect_false(is.null(list.stack(x)))
})

test_that("list.match", {
  # simple list
  x <- list(a = 1, b = 2)
  expect_identical(list.match(x, "a"), x["a"])
  expect_identical(list.match(x, "[ab]"), x)

  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_identical(list.match(x, "p[12]"), x[c("p1", "p2")])
})



test_that("list.take, list.skip", {
  # simple list
  x <- list(a = 1, b = 2)
  expect_identical(list.take(x, 1), x[1])
  expect_identical(list.take(x, 0), x[0])
  expect_identical(list.take(x, -1), x[-1])
  expect_identical(list.skip(x, 1), x[2])
  expect_identical(list.skip(x, 0), x)
  expect_identical(list.skip(x, -1), x[1])
})

test_that("list.takeWhile, list.skipWhile", {
  # simple list
  x <- list(a = 1, b = 2)
  expect_identical(list.takeWhile(x, . <= 1), x[1])
  expect_equal(length(list.takeWhile(x, . >= 3)), 0)
  lapply(1:3, function(i) list.takeWhile(x, . <= i))
  expect_error(list.takeWhile(x, . >= p))
  expect_identical(list.skipWhile(x, . <= 1), x[2])
  expect_equal(length(list.skipWhile(x, . >= 3)), 0)
  lapply(1:3, function(i) list.skipWhile(x, . <= i))
  expect_error(list.skipWhile(x, . >= p))
})

test_that("list.remove", {
  x <- list(a = 1, b = 2)
  expect_identical(list.remove(x, c(FALSE, TRUE)), x[1])
  expect_identical(list.remove(x, 1), x[2])
  expect_identical(list.remove(x, "b"), x["a"])
  expect_identical(list.remove(x, c("a", "b")), x[0])
})

test_that("list.exclude", {
  x <- list(a = 1, b = 2)
  expect_identical(list.exclude(x, . >= 2), x[1])
})

test_that("list.sample", {
  x <- list(a = 1, b = 2, c = 3)
  expect_equal(length(list.sample(x, 2, weight = .)), 2)
  lapply(1:2, function(n) list.sample(x, n))
})

test_that("list.cases", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_equal(list.cases(x, type, sort = T), c("A", "B"))
  expect_equal(list.cases(x, mean(unlist(score))), c(8, 9))
  expect_equal(lapply(c("A", "B"), function(i) list.cases(x, type == i)),
    list(c(FALSE, TRUE), c(FALSE, TRUE)))
  x <- list(x = LETTERS[1:3], y = LETTERS[3:5])
  expect_equal(list.cases(x), Reduce(union, x))
})

test_that("list.common", {
  x <- list(c("a", "b", "c"), c("a", "b"), c("b", "c"))
  expect_equal(list.common(x, .), c("b"))

  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_equal(list.common(x, names(.)), c("type", "score"))
  expect_equal(list.common(x, names(score)), c("c1", "c2"))

  x <- list(x = LETTERS[1:3], y = LETTERS[3:5])
  expect_equal(list.common(x), Reduce(intersect, x))
})

test_that("list.all", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_equal(list.all(x, type == "B"), FALSE)
  expect_equal(list.all(x, mean(unlist(score)) >= 6), TRUE)
  expect_equal(sapply(8:10, function(i) list.all(x, score$c1 >= i)), c(TRUE, TRUE, FALSE))
  expect_equal(list.all(logical()), all())
  expect_equal(list.all(logical(), na.rm = TRUE), all(na.rm = TRUE))
  expect_equal(list.all(c(TRUE, NA, TRUE)), all(c(TRUE, NA, TRUE)))
  expect_equal(list.all(c(TRUE, NA, FALSE)), all(c(TRUE, NA, FALSE)))
  expect_equal(list.all(c(TRUE, NA, TRUE), na.rm = TRUE), all(c(TRUE, NA, TRUE),
    na.rm = TRUE))
  expect_equal(list.all(c(TRUE, NA, FALSE), na.rm = TRUE), all(c(TRUE, NA, FALSE),
    na.rm = TRUE))
  expect_equal(list.all(list(c(1,2,3),c(2,3,4)), . <= 3, na.rm = FALSE), NA)
  expect_equal(list.all(list(1,-2,10), x ~ x > 0), FALSE)
  expect_equal(list.all(list(1,-2,10), x ~ x + 10 > 0), TRUE)
  expect_error(list.all(list(1,2,3), . > p))
})

test_that("list.any", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_equal(list.any(x, type == "B"), TRUE)
  expect_equal(list.any(x, mean(unlist(score)) >= 20), FALSE)
  expect_equal(sapply(8:10, function(i) list.any(x, score$c1 >= i)), c(T, T, T))
  expect_equal(list.any(logical()), any())
  expect_equal(list.any(logical(), na.rm = TRUE), any(na.rm = TRUE))
  expect_equal(list.any(c(TRUE, NA, TRUE)), any(c(TRUE, NA, TRUE)))
  expect_equal(list.any(c(TRUE, NA, FALSE)), any(c(TRUE, NA, FALSE)))
  expect_equal(list.any(c(TRUE, NA, TRUE), na.rm = TRUE), any(c(TRUE, NA, TRUE),
    na.rm = TRUE))
  expect_equal(list.any(c(TRUE, NA, FALSE), na.rm = TRUE), any(c(TRUE, NA, FALSE),
    na.rm = TRUE))
  expect_equal(list.any(list(1,-2,10), x ~ x > 0), TRUE)
  expect_equal(list.any(list(1,-2,10), x ~ x - 10 > 0), FALSE)
  expect_error(list.any(list(1,2,3), . > p))
})

test_that("list.first", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_equal(list.first(x, type == "B"), x[[2L]])
  expect_equal(list.first(x, unlist(score$c1 <= 9)), x[[2L]])
  expect_identical(list.first(x, score$c1 < 9 || score$c3 >= 5), NULL)
  expect_equal(list.first(c(NA, NA, 1), . <= 1), 1)
  expect_error(list.first(list(1,2,3), . > p))
})

test_that("list.last", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_equal(list.last(x, type == "B"), x[[3L]])
  expect_equal(list.last(x, unlist(score$c1 <= 9)), x[[3L]])
  expect_identical(list.last(x, score$c1 < 9 || score$c3 >= 5), NULL)
  expect_error(list.last(list(1,2,3), . > p))
})

test_that("list.table", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  x.types <- c("A", "B", "B")
  x.c1 <- c(10, 9, 9)
  expect_identical(list.table(x, type), table(type = x.types))
  expect_identical(list.table(x, type, c1 = score$c1), table(type = x.types, c1 = x.c1))

  x <- list(list(a = 1, b = NULL), list(a = 2, b = 1), list(a = 3, b = 1))
  expect_identical(as.integer(list.table(x, a, b)), c(0L, 1L, 1L, 1L, 0L, 0L))
})

test_that("list.zip", {
  a <- list(1, 2)
  b <- list("a", "b")
  expect_identical(list.zip(a, b), list(list(a = 1, b = "a"), list(a = 2, b = "b")))
})

test_that("list.unzip", {
  x <- list(p1 = list(a = 1, b = 5), p2 = list(a = 2, b = 3))
  x1 <- list(p1 = list(a = 1, b = 5), p2 = list(a = 2, b = 3, c = 4))
  expect_identical(list.unzip(x), list(a = c(p1 = 1, p2 = 2), b = c(p1 = 5, p2 = 3)))
  expect_identical(list.unzip(x1, .fields = "union"), list(a = c(p1 = 1, p2 = 2),
    b = c(p1 = 5, p2 = 3), c = c(p1 = NA, p2 = 4)))
  expect_identical(list.unzip(x, a = "identity"), list(a = list(p1 = 1, p2 = 2),
    b = c(p1 = 5, p2 = 3)))
  expect_identical(list.unzip(x, a = NULL), list(b = c(p1 = 5, p2 = 3)))
})

test_that("list.flatten", {
  p <- list(a = 1, b = list(b1 = 2, b2 = 3), c = list(c1 = list(c11 = "a", c12 = "x"),
    c2 = 3))
  q <- list(a = c(1, 2, 3), b = list(x = 1, y = list(z = 1, z2 = 2)))
  expect_identical(list.flatten(p), list(a = 1, b.b1 = 2, b.b2 = 3, c.c1.c11 = "a",
    c.c1.c12 = "x", c.c2 = 3))
  expect_identical(list.flatten(q), list(a = c(1, 2, 3), b.x = 1, b.y.z = 1, b.y.z2 = 2))

  p <- list(a=1,b=list(x="a",y="b",z=10))
  expect_identical(list.flatten(p), list(a=1, b.x = "a", b.y = "b", b.z = 10))
  expect_identical(list.flatten(p, classes = "numeric"), list(a = 1, b.z = 10))
  expect_identical(list.flatten(p, classes = "character"), list(b.x = "a", b.y = "b"))
  expect_identical(list.flatten(p, classes = "integer"), list())
  expect_identical(list.flatten(p, use.names = FALSE), list(1, "a", "b", 10))
})

test_that("list.names", {
  expect_identical(list.names(list(a = 1, b = 2)), c("a", "b"))
  expect_identical(list.names(list(1, 2)), NULL)
  expect_identical(list.names(list(1, 2), letters[.]), list(a = 1, b = 2))
  expect_identical(list.names(list(a = 1, b = 2), NULL), list(1, 2))
})

test_that("list.parse", {
  expect_identical(list.parse(c(a = 1)), list(a = 1))
  expect_identical({
    mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
    rownames(mat) <- c("a", "b", "c")
    colnames(mat) <- paste0("V", 1:2)
    list.parse(mat)
  }, list(a = list(V1 = 1, V2 = 4), b = list(V1 = 2, V2 = 5), c = list(V1 = 3,
    V2 = 6)))
  expect_identical(list.parse("hello"), list("hello"))
  expect_identical(list.parse(data.frame(x = c(1, 2), y = c(2, 3))), list(`1` = list(x = 1,
    y = 2), `2` = list(x = 2, y = 3)))
  expect_equal(list.parse("a: 1", "yaml"), list(a = 1))
  expect_equal(list.parse("{ \"a\": 1, \"b\": 2 }", "json"), list(a = 1, b = 2))
  expect_equal(list.parse("<root><a>1</a><b>2</b></root>", "xml"), list(a = "1",
    b = "2"))
  expect_error(list.parse("a:1,b:2", "js"), "Unsupported type of data")
  expect_equal(list.parse(c("a: 1", "{ \"a\": 1, \"b\": 2 }"), c("yaml", "json")),
    list(list(a = 1), list(a = 1, b = 2)))
})

test_that("list.clean", {
  expect_identical(list.clean(list(1, 2, NULL)), list(1, 2))
  expect_identical(list.clean(list(1, 2, NA), is.na), list(1, 2))
  expect_identical(list.clean(list(1, 2, numeric(), list(1, 2, NULL)), function(x) length(x) ==
    0L, recursive = TRUE), list(1, 2, list(1, 2)))
  expect_identical(list.clean(list(1, 2, list(1, 2, NULL), NULL), recursive = TRUE),
    list(1, 2, list(1, 2)))
})

test_that("list.which", {
  x <- c(1, 2, 3)
  expect_identical(list.which(x, . >= 2), which(x >= 2))
})

test_that("list.expand", {
  expect_identical(list.expand(), list())
  expect_identical(list.expand(x = integer()), list())
  expect_identical(list.expand(x = 1:3, y = integer()), list())
  expect_identical(list.expand(x = c(1,2), y = c(2,3)),
    list(list(x = 1, y = 2), list(x = 2, y = 2),
      list(x = 1, y = 3), list(x = 2, y = 3)))
})
