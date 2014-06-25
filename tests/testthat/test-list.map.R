context("list mapping")

test_that("list.map", {

  # simple list
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_identical(list.map(x,score$c1),list(p1=10,p2=9,p3=9))

  # list of vectors
  x <- list(a=c(x=1,y=2),b=c(x=3,y=4))
  expect_identical(list.map(x,sum(.)),list(a=3,b=7))
})
