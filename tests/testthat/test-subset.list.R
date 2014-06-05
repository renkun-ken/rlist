context("list subsetting")

test_that("list subsetting works as expected", {

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_that(subset(x,type=="B"),is_identical_to(x[c(2,3)]))
  expect_that(subset(x,type=="B",score$c1),is_identical_to(list(p2=9,p3=9)))

})
