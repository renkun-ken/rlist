context("list subsetting")

test_that("list subsetting works as expected", {

  # simple list
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_identical(subset(x,type=="B"),x[c(2,3)])
  expect_identical(subset(x,type=="B",score$c1),list(p2=9,p3=9))

})
