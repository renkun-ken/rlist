context("io")

test_that("list.serialize", {
  x <- list(a = 1, b = 2, c = 3, d = list(x = 1, y = 2))
  f <- tempfile(fileext = ".json")
  expect_identical(list.serialize(x, f), x)
  expect_identical(list.unserialize(f), x)
  file.remove(f)

  f <- tempfile()
  expect_identical(list.serialize(x, f), x)
  expect_identical(list.unserialize(f), x)
  file.remove(f)
})

test_that("list.save, list.load", {
  x <- list(a = 1, b = 2, c = 3, d = list(x = 1, y = c(1, 2, 3)))

  f <- tempfile(fileext = ".json")
  expect_equal(list.save(x, file = f), x)
  expect_equal(list.load(f), x)
  file.remove(f)

  f <- tempfile(fileext = ".yaml")
  expect_equal(list.save(x, file = f), x)
  expect_equal(list.load(f), x)
  file.remove(f)

  f <- tempfile(fileext = ".rds")
  expect_equal(list.save(x, file = f), x)
  expect_equal(list.load(f), x)
  file.remove(f)

  f <- tempfile(fileext = ".rdata")
  expect_equal(list.save(x, file = f), x)
  expect_equal(list.load(f), x)
  file.remove(f)

  f <- tempfile(fileext = ".xml")
  writeLines("<root><a>1</a></root>", f)
  expect_equal(list.load(f), list(a = "1"))
  file.remove(f)

  # guess
  f <- tempfile()
  expect_equal(list.save(x, file = f, type = "yaml"), x)
  expect_equal(list.load(f), x)
  file.remove(f)

  f <- tempfile()
  expect_error(list.save(x, file = f, type = "unsupported_file_type"))

  f <- tempfile()
  expect_error(list.load(f, "unsupported_file_type"))

  n <- 3
  fs <- vapply(seq_len(n), function(i) tempfile(fileext = ".json"), character(1L))
  ds <- lapply(seq_len(n), function(i) list(a = i, b = i + 1L))
  Map(function(d, f) {
    list.save(d, f)
  }, ds, fs)
  expect_equal(unname(list.load(fs)), ds)
  expect_equal(list.load(fs, action = "ungroup"), list.ungroup(lapply(fs, list.load)))
  expect_equal(list.load(fs, action = "merge"), do.call("list.merge", ds))
  file.remove(fs)
})
