context("list.filter")

test_that("list.filter", {

  # simple list
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_identical(list.filter(x,type=="B"),x[c(2,3)])

  # list of vectors
  x <- list(a=c(x=1,y=2),b=c(x=3,y=4))
  expect_identical(list.filter(x,sum(.)>=4),x["b"])

  # list of lists

  l1 <- list(a=list(x=1,y=2),b=list(x=2,y=3))
  expect_identical(list.filter(l1,sum(unlist(.)) <= 4),l1["a"])

})
