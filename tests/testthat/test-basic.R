context("basic functions")

test_that("list.append", {

  # simple list
  x <- list(a=1,b=2)
  expect_identical(list.append(x,c=3),c(x,c=3))
  expect_identical(list.append(x,list(c=3)),c(x,c=3))

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  p4 <- list(type="A",score=list(c1=10,ce=6))
  expect_identical(list.append(x,p4=list(p4)),c(x,p4=list(p4)))
})

test_that("list.prepend", {

  # simple list
  x <- list(a=1,b=2)
  expect_identical(list.prepend(x,c=3),c(c=3,x))
  expect_identical(list.prepend(x,list(c=3)),c(c=3,x))

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  p0 <- list(type="A",score=list(c1=10,ce=6))
  expect_identical(list.prepend(x,p0=list(p0)),c(p0=list(p0),x))
})

test_that("list.extract", {

  # simple list
  x <- list(a=1,b=2,c=3)
  expect_identical(list.extract(x,1),x[[1]])
  expect_identical(list.extract(x,"a"),x[["a"]])
})

test_that("list.select", {
  # simple list
  x <- list(a=1,b=2,c=3)
  expect_identical(list.select(x,1),x[1])
  expect_identical(list.select(x,"a"),x["a"])
})

test_that("list.count", {

  # simple list
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_equal(list.count(x,type=="B"),2)
  expect_equal(list.count(x,score$c1<10),2)


  # list of vectors
  x <- list(a=c(x=1,y=2),b=c(x=3,y=4))
  expect_equal(list.count(x,sum(.)>=3),2)
  expect_equal(list.count(x,mean(.)>=3),1)

})

test_that("list.reverse", {

  # simple list
  x <- list(a=1,b=2,c=3)
  expect_identical(list.reverse(x),x[c(3,2,1)])

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_identical(list.reverse(x),x[c(3,2,1)])
})

test_that("list.merge", {

  # simple list
  x <- list(a=1,b=2,c=list(x=1,y=2))
  expect_identical(list.merge(x,list(b=5)),list(a=1,b=5,c=list(x=1,y=2)))
  expect_identical(list.merge(x,list(c=list(z=3))),list(a=1,b=2,c=list(x=1,y=2,z=3)))

})
