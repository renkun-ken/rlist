context("list grouping")

test_that("list.group", {

  # simple list
  x <- list(a=1,b=2,c=3,d=2,e=3,f=1)
  expect_identical(list.group(x,.),list(`1`=list(a=1,f=1),`2`=list(b=2,d=2),`3`=list(c=3,e=3)))

  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))
  expect_identical(list.group(x,type),list(A=x["p1"],B=x[c("p2","p3")]))
  expect_identical(list.group(x,mean(unlist(score))),list(`8`=x["p3"],`9`=x[c("p1","p2")]))

  lapply(2:4,function(i) list.group(x,sum(unlist(score))<=i))
})

test_that("list.ungroup",{
  x <- list(a=1,b=2,c=3,d=2,e=3,f=1)
  xg <- list(`1`=list(a=1,f=1),`2`=list(b=2,d=2),`3`=list(c=3,e=3))
  expect_identical(list.ungroup(xg,sort.names = TRUE),x)
})
