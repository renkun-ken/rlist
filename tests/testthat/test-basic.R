context("basic functions")

test_that("list.append", {

  # simple list
  x <- list(a=1,b=2)
  expect_identical(list.append(x,c=3),c(x,c=3))
  expect_identical(lapply(1:2,function(i) list.append(x,d=i)),
    lapply(1:2,function(i) c(x,d=i)))

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  p4 <- list(type="A",score=list(c1=10,ce=6))
  expect_identical(list.append(x,p4=p4),c(x,p4=list(p4)))
})

test_that("list.prepend", {

  # simple list
  x <- list(a=1,b=2)
  expect_identical(list.prepend(x,c=3),c(c=3,x))
  expect_identical(lapply(1:2,function(i) list.prepend(x,d=i)),
    lapply(1:2,function(i) c(list(d=i),x)))

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  p0 <- list(type="A",score=list(c1=10,ce=6))
  expect_identical(list.prepend(x,p0=p0),c(p0=list(p0),x))
})

test_that("list.insert", {
  x <- list(a=1,b=2,c=3)
  expect_identical(list.insert(x,2,q=0),list(a=1,q=0,b=2,c=3))
  expect_identical(lapply(1:2,function(i) list.insert(x,2,q=i)),
    lapply(1:2,function(i) list(a=1,q=i,b=2,c=3)))
})

test_that("list.extract", {

  # simple list
  x <- list(a=1,b=2,c=3)
  expect_identical(list.extract(x,1),x[[1]])
  expect_identical(list.extract(x,"a"),x[["a"]])
  expect_identical(lapply(1:2,function(i) list.extract(x,i)),
    lapply(1:2,function(i) x[[i]]))
})

test_that("list.subset", {
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_identical(list.subset(x, c("p1","p2")),x[c("p1","p2")])
  expect_identical(list.subset(x, "^p", pattern = TRUE),x[])
  expect_identical(list.subset(x, "x1", dist = 1),x["p1"])
})

test_that("list.count", {

  # simple list
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_equal(list.count(x,type=="B"),2)
  expect_equal(list.count(x,score$c1<10),2)
  expect_equal(lapply(c(8,9,10),function(i) list.count(x,score$c1<=i)),
    list(0,2,3))

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

  # multiple lists
  l1 <- list(a=1,b=list(x=1,y=1))
  l2 <- list(a=2,b=list(z=2))
  l3 <- list(a=2,b=list(x=3))
  expect_identical(list.merge(l1,l2,l3),
    modifyList(modifyList(l1,l2),l3))

})

test_that("list.rbind", {
  x <- lapply(1:10,function(i) c(a=i,b=i^2))
  expect_identical(list.rbind(x),do.call(rbind,x))
})

test_that("list.cbind", {
  x <- list(
    data.frame(a=rnorm(10),b=rnorm(10)),
    data.frame(c=rnorm(10),d=rnorm(10)),
    data.frame(e=rnorm(10),f=rnorm(10)))
  expect_identical(list.cbind(x),do.call(cbind,x))
})

test_that("list.stack", {
  x <- lapply(1:10,function(i) list(a=i,b=i^2))
  expect_false(is.null(list.stack(x)))
  x <- lapply(1:10,function(i) list(a=c(i,i+1),b=c(i^2,i^2+1)))
  expect_false(is.null(list.stack(x)))
})

test_that("list.match", {
  # simple list
  x <- list(a=1,b=2)
  expect_identical(list.match(x,"a"),x["a"])
  expect_identical(list.match(x,"[ab]"),x)

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_identical(list.match(x,"p[12]"),x[c("p1","p2")])
})



test_that("list.take, list.skip", {
  # simple list
  x <- list(a=1,b=2)
  expect_identical(list.take(x,1),x[1])
  expect_identical(list.skip(x,1),x[2])
})

test_that("list.takeWhile, list.skipWhile", {
  # simple list
  x <- list(a=1,b=2)
  expect_identical(list.takeWhile(x,.<=1),x[1])
  expect_equal(length(list.takeWhile(x,.>=3)),0)
  lapply(1:3,function(i)list.takeWhile(x,. <= i))
  expect_identical(list.skipWhile(x,.<=1),x[2])
  expect_equal(length(list.skipWhile(x,.>=3)),0)
  lapply(1:3,function(i)list.skipWhile(x,. <= i))
})

test_that("list.remove", {
  x <- list(a=1,b=2)
  expect_identical(list.remove(x,1),x[2])
  expect_identical(list.remove(x,"b"),x["a"])
  expect_identical(list.remove(x,c("a","b")),x[0])
})

test_that("list.sample", {
  x <- list(a=1,b=2,c=3)
  expect_equal(length(list.sample(x,2,weight = .)),2)
  lapply(1:2,function(n) list.sample(x,n))
})

test_that("list.cases", {
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_equal(list.cases(x,type,sort=T),c("A","B"))
  expect_equal(list.cases(x,mean(unlist(score))),c(8,9))
  lapply(c("A","B"),function(i) list.cases(x,type==i))
})

test_that("list.common", {
  x <- list(c("a","b","c"),c("a","b"),c("b","c"))
  expect_equal(list.common(x,.),c("b"))

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_equal(list.common(x,names(.)),c("type","score"))
  expect_equal(list.common(x,names(score)),c("c1","c2"))
})

test_that("list.all", {
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_equal(list.all(x,type=="B"),FALSE)
  expect_equal(list.all(x,mean(unlist(score))>=6),TRUE)
  expect_equal(sapply(8:10,function(i) list.all(x,score$c1>=i)),c(T,T,F))
})

test_that("list.any", {
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_equal(list.any(x,type=="B"),TRUE)
  expect_equal(list.any(x,mean(unlist(score))>=20),FALSE)
  expect_equal(sapply(8:10,function(i) list.any(x,score$c1>=i)),c(T,T,T))
})

test_that("list.table", {
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  x.types <- c("A","B","B")
  x.c1 <- c(10,9,9)
  expect_identical(list.table(x,type),table(type=x.types))
  expect_identical(list.table(x,type,c1=score$c1),table(type=x.types,c1=x.c1))
})

test_that("list.zip", {
  a <- list(1,2)
  b <- list("a","b")
  expect_identical(list.zip(a,b),list(list(a=1,b="a"),list(a=2,b="b")))
})

test_that("list.flatten", {
  p <- list(a=1,b=list(b1=2,b2=3),c=list(c1=list(c11="a",c12="x"),c2=3))
  expect_identical(list.flatten(p),list(a=1,b.b1=2,b.b2=3,c.c1.c11="a",
    c.c1.c12="x",c.c2=3))
})
