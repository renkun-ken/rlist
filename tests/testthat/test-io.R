context("input/output")

test_that("list.serialize", {
  x <- list(a=1,b=2,c=3,d=list(x=1,y=2))
  f <- tempfile(fileext = ".json")
  expect_identical(list.serialize(x, f), x)
  expect_identical(list.unserialize(f), x)
  file.remove(f)

  f <- tempfile()
  expect_identical(list.serialize(x, f), x)
  expect_identical(list.unserialize(f), x)
  file.remove(f)
})
