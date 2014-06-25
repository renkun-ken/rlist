context("transform.list")

test_that("transform.list", {

  # simple list
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  x1 <- list(p1 = list(type="A",score=list(c1=10,c2=8),high=10,low=8),
    p2 = list(type="B",score=list(c1=9,c2=9),high=9,low=9),
    p3 = list(type="B",score=list(c1=9,c2=7),high=9,low=7))
  x2 <- list(p1 = list(type="A",score=list(c1=10,c2=8,mean=9)),
    p2 = list(type="B",score=list(c1=9,c2=9,mean=9)),
    p3 = list(type="B",score=list(c1=9,c2=7,mean=8)))

  expect_identical(
    transform(x,high=max(score$c1,score$c2),low=min(score$c1,score$c2)),x1)
  expect_identical(
    transform(x,score=list(mean=mean(unlist(score)))),x2)

  # list of vectors

  # list of lists

  # list of objects of list mode

  # list of S4 objects

  # list of other objects

})
