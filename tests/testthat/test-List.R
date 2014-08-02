context("List")

test_that("List", {
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_identical({
    List(x)$group(type)$
      map(g -> List(g)$
          map(score)$
          .(unlist)$
          .(mean)$
          data)$
      data
  },list(A=9,B=8.5))
})
