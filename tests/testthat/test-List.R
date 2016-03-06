context("List")

test_that("List", {
  x <- list(p1 = list(type = "A", score = list(c1 = 10, c2 = 8)), p2 = list(type = "B",
    score = list(c1 = 9, c2 = 9)), p3 = list(type = "B", score = list(c1 = 9,
    c2 = 7)))
  expect_identical({
    List(x)$group(type)$map(g ~ List(g)$map(score)$call(unlist)$call(mean)$data)$data
  }, list(A = 9, B = 8.5))
  expect_identical({
    local({
      i <- 3
      List(1:3)$map(x ~ x + i)[]
    })
  }, list(4, 5, 6))
  expect_identical({
    local({
      i <- 1
      List(1:3)$map(x ~ x + i)$filter(x ~ x <= 1 + i)[]
    })
  }, list(2))
  expect_identical(List(1:3)$all(. >= 0)[], TRUE)
  expect_identical(List(1:3)$any(. <= 5)[], TRUE)
  expect_identical(List(x)$cases(type)[], c("A", "B"))
  expect_identical(List(x)$class(type)[], list.class(x, type))
  expect_identical(List(x)$common(names(score))[], c("c1", "c2"))
  expect_identical(List(x)$count(type == "B")[], 2L)
  expect_identical(List(x)$filter(type == "B")[], list.filter(x, type == "B"))
  expect_identical(List(1:10)$find(.%%2 == 0, 3)[], c(2L, 4L, 6L))
  expect_identical(List(1:10)$findi(.%%2 == 0, 3)[], c(2L, 4L, 6L))
  expect_identical(List(1:10)$group(.%%3)[], list.group(1:10, .%%3))
  expect_identical(List(list(1:10, 2:15))$search(any(. >= 11))[], list.search(list(1:10,
    2:15), any(. >= 11)))
  expect_identical(local({
    i <- 12
    List(list(1:10, 2:15))$search(any(. >= i))[]
  }), list.search(list(1:10, 2:15), any(. >= 12)))
  expect_identical(List(1:200)$table(.%%2, .%%3)[], list.table(1:200, .%%2, .%%3))
  expect_identical(List(x)$update(mean = mean(unlist(score)))[], list.update(x,
    mean = mean(unlist(score))))
  expect_equal(List(c(1, 2, 3)) == c(1, 2, 3), c(TRUE, TRUE, TRUE))
  expect_equal(subset(List(1:10), c(TRUE, FALSE)), subset(1:10, c(TRUE, FALSE)))
})

test_that("closure", {
  expect_is(List_get_function(quote(`[`)), "function")
  expect_is(List_set_function(quote(`[`)), "function")
})

test_that("subsetting", {
  expect_identical(List(list(a = 1, b = 2))["a"]$data, list(a = 1))
  expect_equal(List(c(a = 1, b = 2))["a"]$data, c(a = 1))
})

test_that("extracting", {
  expect_equal(List(list(a = 1, b = 2))[["a"]]$data, 1)
  expect_equal(List(list2env(list(a = 1, b = 2)))[["a"]]$data, 1)
})

test_that("assignment", {
  expect_identical({
    z <- List(list(a = 1, b = 2))
    z$a <- 2
    z$b <- NULL
    z$data
  }, list(a = 2))
  expect_equal({
    z <- new.env()
    env <- List(z)
    env$a <- 1
    env$data$a
  }, 1)
  expect_identical({
    z <- List(c(a = 1, b = 2))
    z["a"] <- 2
    z$data
  }, c(a = 2, b = 2))
  expect_identical({
    z <- List(c(a = 1, b = 2))
    z[["a"]] <- 2
    z$data
  }, c(a = 2, b = 2))
})

test_that("printing", {
  expect_output(print(List(list(1, 2, 3))), "\\$data : list.+")
  expect_output(str(List(list(1, 2, 3))), "\\$data : List of 3.+")
  expect_output(print(summary(List(rnorm(100)))), "Min.+")
})
