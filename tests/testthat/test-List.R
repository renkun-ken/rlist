context("List")

test_that("List", {
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_identical({
    List(x)$group(type)$
      map(g -> List(g)$
          map(score)$
          call(unlist)$
          call(mean)$
          data)$
      data
  },list(A=9,B=8.5))
  expect_identical({
    local({
      i <- 3
      List(1:3)$map(x -> x+i) []
    })
  },list(4,5,6))
  expect_identical({
    local({
      i <- 1
      List(1:3)$map(x -> x + i)$filter(x -> x <= 1 + i)[]
    })
  }, list(2))
  expect_identical(List(1:3)$all(. >= 0)[], TRUE)
  expect_identical(List(1:3)$any(. <= 5)[], TRUE)
  expect_identical(List(x)$cases(type)[], c("A","B"))
  expect_identical(List(x)$class(type)[], list.class(x,type))
  expect_identical(List(x)$common(names(score))[], c("c1","c2"))
  expect_identical(List(x)$count(type == "B")[], 2L)
  expect_identical(List(x)$filter(type == "B")[], list.filter(x,type=="B"))
})
