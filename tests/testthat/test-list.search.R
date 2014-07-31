context("list.search")

test_that("list.search", {

  # exact search
  x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
    p2 = list(type="B",score=list(c1=9,c2=9)),
    p3 = list(type="B",score=list(c1=9,c2=7)))

  expect_identical(list.search(x,all,identical,"A"),
    list(p1=list(type="A")))
  expect_identical(list.search(x,all,identical,"A",unlist = TRUE),
    c(p1.type="A"))
  expect_identical(list.search(x,all,identical,9),
    list(p2=list(score=list(c1=9,c2=9)),p3=list(score=list(c1=9))))

  x <- list(p1 = list(x=c("A","B","C"),y=list(y1="A",y2=c("B","C"))),
    p2 = list(a=c("A","B"),b=list(b1=c("B","C"),b2=list("C","B"))))

  expect_identical(list.search(x,all,equal,"A"),
    list(p1=list(y=list(y1="A"))))
  expect_identical(list.search(x,any,equal,c("B","A")),
    list(p1=list(y=list(y2=c("B","C"))),
      p2=list(b=list(b1=c("B","C")))))

  expect_identical(list.search(x,any,include,"A"),
    list(p1=list(x=c("A","B","C"),y=list(y1="A")),
      p2=list(a=c("A","B"))))

  # fuzzy search
  x <- list(
    p1 = list(name="Ken",age=24),
    p2 = list(name="Kent",age=26),
    p3 = list(name="Sam",age=24),
    p4 = list(name="Keynes",age=30),
    p5 = list(name="Kwen",age=31))
  expect_equal(list.search(x,any, like(1),"Ken",unlist = TRUE),
    c(p1.name="Ken",p2.name="Kent",p5.name="Kwen"))
  expect_identical(list.search(x,all, like(0L),"Ken"),
    list(p1=list(name="Ken")))

  x <- list(
    p1 = list(name=c("Ken", "Ren"),age=24),
    p2 = list(name=c("Kent", "Potter"),age=26),
    p3 = list(name=c("Sam", "Lee"),age=24),
    p4 = list(name=c("Keynes", "Bond"),age=30),
    p5 = list(name=c("Kwen", "Hu"),age=31))
  expect_equal(list.search(x,all,like(1),"Ken"),
    list(p1=list(name=c("Ken","Ren"))))
  expect_equal(list.search(x,any,like(1),"Ken"),
    list(p1=list(name=c("Ken","Ren")),
      p2=list(name=c("Kent","Potter")),
      p5=list(name=c("Kwen","Hu"))))
  expect_equal(list.search(x,all,unlike(2),"Ken"),
    list(p3=list(name=c("Sam","Lee")),
      p4=list(name=c("Keynes", "Bond"))))
})
